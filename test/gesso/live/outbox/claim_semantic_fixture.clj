(ns gesso.live.outbox.claim-semantic-fixture
  "Independent typed source for the installed two-helper Claim fixture.
   One initial epoch and one command per helper; no production writer closure.
   Observations are diagnostic, not certificates or transaction authority."
  (:require [clojure.set :as set]
            [gesso.assurance.transition :as construction]
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

(defn snapshot!
  "Project actual rows supplied by a query function. Use snapshot-at-frontier!
   for physical observations; this decoder does not itself pin any reads."
  [q]
  (let [requests (q ["SELECT _id, status, revision FROM requests"])
        assignments (q ["SELECT _id, request_id, helper FROM request_assignments"])
        memberships (q ["SELECT _id, active FROM memberships"])
        records (q ["SELECT * FROM gesso_commands"])
        request (first requests)
        assignment (first assignments)]
    (require! (and (= 1 (count requests)) (= "r1" (:xt/id request))
                   (<= (count assignments) 1)
                   (or (empty? assignments)
                       (and (= "primary-r1" (:xt/id assignment))
                            (= "r1" (:request-id assignment)) (helpers (:helper assignment))))
                   (= 2 (count memberships)) (= (set (keys helpers)) (set (map :xt/id memberships)))))
    (let [state (reduce (fn [s {:keys [xt/id active]}]
                          (assoc s (second (helpers id)) active))
                        {:request/status (:status request) :request/revision (:revision request)
                         :assignment/helper (:helper assignment) :journal/one nil :journal/two nil}
                        memberships)]
      (reduce (fn [s record]
                (let [binding (:gesso.command/binding record)
                      helper (identity/raw-value (:principal binding))
                      slot (first (helpers helper))
                      outcome (:gesso.command/outcome record)]
                  (require! (and slot (nil? (get s slot))
                                 (= :request/claim (:operation binding))
                                 (= {:request-id "r1"} (:arguments binding))
                                 (= (:xt/id record) (identity/raw-value (:command-id binding)))
                                 (= :incorporated (:gesso.command/status record))))
                  (assoc s slot
                         (cond (= {:outcome :request/claimed :helper helper} outcome) :accepted
                               (= {:outcome :request/not-claimable :reason :already-claimed} outcome) :rejected
                               :else (throw (ex-info "Unknown Claim outcome" {}))))))
              state records))))

(defn snapshot-at-frontier!
  "Capture one completed basis on the selected node and use that exact
   system-time snapshot and a single valid-time clock for every projection
   query. This observes a completed frontier, not transaction-internal state
   or evidence of complete writer coverage."
  [ctx]
  (let [node (xtdb/connectable-from ctx)
        {:keys [basis]} (frontier/q-with-basis! node ["SELECT 1 AS ready"])
        opts {:snapshot-token (xtdb/basis-snapshot-token basis)
              :current-time (Instant/now)}]
    {:basis basis :state (snapshot! #(xtdb/q-from node % opts))}))

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
                            (= {:xt/id "r1" :status :claimed :revision 1}
                               (select-keys (first requests) [:xt/id :status :revision]))
                            (= {:xt/id "primary-r1" :request-id "r1" :helper helper}
                               (select-keys (first assignments) [:xt/id :request-id :helper]))
                            (= 2 (count intents) (count (set (map :xt/id intents))))
                            (= #{:request :request-assignment} (set (map :gesso.live.outbox/topic intents)))
                            (every? #(and (= id (:gesso.live.outbox/command-id %))
                                          (= "l1" (:gesso.live.outbox/subject-id %))) intents)))
      :refuse (require! (= #{:gesso_commands} (set (keys by-table))))
      (require! false))
    {:alternative (alternative-id helper alternative) :command-id id}))

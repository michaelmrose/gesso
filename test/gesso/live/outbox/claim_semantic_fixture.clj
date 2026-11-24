(ns gesso.live.outbox.claim-semantic-fixture
  "Independent typed source for the installed two-helper Claim fixture.
   One initial epoch and one command per helper; no production writer closure.
   Observations are diagnostic, not certificates or transaction authority."
  (:require [clojure.set :as set]
            [gesso.assurance.transition :as construction]
            [gesso.assurance.proof.transition :as abstract]
            [gesso.assurance.proof.independent-transition-semantics :as independent]
            [gesso.choreo.identity :as identity]))

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

(defn source [helper alternative]
  (let [[slot membership] (or (helpers helper) (throw (ex-info "Unknown helper" {})))
        accept? (= :accept alternative)
        _ (when-not (#{:accept :refuse} alternative) (throw (ex-info "Unknown alternative" {})))
        conditions (if accept?
                     {:request/status :open :request/revision 0 :assignment/helper nil
                      slot nil membership true}
                     {:request/status :claimed :request/revision 1 slot nil})
        replacements (if accept?
                       {:request/status [:literal :claimed] :request/revision [:literal 1]
                        :assignment/helper [:literal helper] slot [:literal :accepted]}
                       {slot [:literal :rejected]})
        reads (set (keys conditions))
        writes (set (keys replacements))]
    {:subject {:kind :operation :id :fixture/multi-record-claim
               :interface {:version 1 :helper helper :alternative alternative
                           :request "r1" :assignment "primary-r1"}}
     :state-types state-types :input-types {}
     :guard {:readers reads
             :expression (into [:and] (map (fn [[k v]] [:= [:read k] [:literal v]])
                                           (sort-by key conditions)))}
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
  "Project actual rows at fixture quiescence or inside its sole recovery
   constructor, before the rejection submits. Separate reads are NOT a
   general atomic snapshot; the fixture has no other domain/journal writers."
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

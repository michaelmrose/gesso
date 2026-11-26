(ns gesso.assurance.reference-mapped-host-lifecycle-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [gesso.assurance.reference-source-coverage :as coverage]
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

(deftest assembled-host-owns-mapped-activation-and-request-adapter
  (let [root (Files/createTempDirectory "gesso-mapped-host-root-" (make-array FileAttribute 0))
        source-id "assembled-mapped-root"
        source (claim/physical-mapped-admission-source (claim/bound-inductive-source!))
        admission (induction/build-inductive-mapped-admission! source)
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
                                 history (coverage/observe-local-processed-retained-replay-inputs! node)]
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
                              :adapter adapter :token token :initial initial :observation observation :history history}))
                run! (fn []
                       (let [started (host/start! assembly handler config)]
                         (try (inspect! started) (finally (host/stop! started)))))
                first-run (run!)]
            (is (= [:manifest :initializer] @policy-events))
            (is (= :stopped (:status (lifecycle/status (:worker first-run)))))
            (is (live/closed? (:live first-run)))
            (is (= 503 (:status ((:handler first-run) {:uri "/"}))))
            (is (= :invalid-adapter-connectable (reason #(xtdb/q (:adapter first-run) "SELECT 1"))))
            (is (nil? (correspondence/initialization-observation (:node first-run) admission)))
            (let [second-run (run!)]
              (is (= [:manifest :initializer] @policy-events))
              (is (not (identical? (:token first-run) (:token second-run))))
              (is (= (:history first-run) (:history second-run)))
              (is (= (:initial first-run) (:initial second-run)))
              (is (= (:initial first-run)
                     (get-in (publication/read-framed-submission-observation! admission (:observation first-run))
                             [:activation-root :observation])))
              (is (= :invalid-adapter-connectable (reason #(xtdb/q (:adapter second-run) "SELECT 1"))))))))
      (finally (doseq [f (reverse (file-seq (.toFile root)))] (io/delete-file f true))))))

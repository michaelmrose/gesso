(ns gesso.assurance.proof.xtdb-poststate-basis-acquisition-test
  "v1522: Real-XTDB adversarial tests of the v1521 pinned physical-domain / 
   committed-ledger read join. A matching ledger row and pinned read basis
   are NOT evidence that the key's transaction caused the observed state.
   These tests deliberately include foreign writers, untracked documents,
   deletion and recreation, and report/handle substitution."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [clojure.string :as str]
            [gesso.assurance.proof.xtdb-poststate-basis-acquisition :as post]
            [gesso.assurance.proof.physical-domain-acquisition :as acquisition]
            [gesso.assurance.proof.xtdb-internal-prestate-capability :as cap]
            [gesso.model.command :as command]
            [xtdb.api :as xt]
            [xtdb.node :as xtn]
            [xtdb.basis :as basis]))

(def ^:private table :gesso_v1522_poststate_probe)
(def ^:private version-keys
  {:revision-key :revision :created-at-key :created-at
   :updated-at-key :updated-at})
(def ^:private typed
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
   :frame #{:request/version :request/note}})
(def ^:private obligation
  {:construction typed
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
(defn- node [] (or @!node (throw (ex-info "No XTDB fixture" {}))))
(defn- with-node [f]
  (with-open [n (xtn/start-node {})]
    (reset! !node n)
    (try (f) (finally (reset! !node nil)))))
(use-fixtures :once with-node)
(defn- fresh-id [] (str "gesso-v1522-" (random-uuid)))
(defn- document [id]
  {:xt/id id :revision 0 :created-at "private-created-v1522"
   :updated-at "private-unchanged-v1522" :value "private-note-v1522"})
(defn- source [id]
  {:obligation-source obligation
   :protected-domain-source
   {:plan {:commands [(command/create table (document id) version-keys)]
           :emit false}
    :protected-documents [{:target [table id] :fields :all}]}
   :field-locations
   {:request/status {:target [table id] :physical-field :revision}
    :request/owner {:target [table id] :physical-field :created-at}
    :request/version {:target [table id] :physical-field :updated-at}
    :request/note {:target [table id] :physical-field :value}}})
(defn- with-frame-only [source id protection]
  (update-in source [:protected-domain-source :protected-documents]
             conj {:target [table id] :fields protection}))
(defn- put! [doc] (xt/execute-tx (node) [[:put-docs table doc]]))
(defn- delete! [id] (xt/execute-tx (node) [[:delete-docs table id]]))
(defn- observe! [s key]
  (post/acquire-poststate-basis! (node) s key))
(defn- rejected [f]
  (try (f) :accepted
       (catch clojure.lang.ExceptionInfo e
         (cond
           (:xtdb-poststate-basis-acquisition/rejection (ex-data e))
           [:poststate (:reason (ex-data e))]
           (:physical-domain-acquisition/rejection (ex-data e))
           [:domain (:reason (ex-data e))]
           :else (throw e)))))
(defn- bounded-only [report handle]
  (let [safe (post/redacted-summary report handle)]
    (is (identical? report (post/require-live-poststate-basis!
                            report handle)))
    (is (= :bounded-external-ledger-diagnostic-only (:proof-status safe)))
    (is (= :caller-supplied-unattested (:node-and-key-origin safe)))
    (is (= :not-established (:physical-poststate-transaction-causality safe)))
    (is (= :not-authenticated (:transaction-internal-prestate safe)))
    (is (= :not-established (:executed-plan-binding safe)))
    (is (= :not-established (:complete-writer-coverage safe)))
    (is (= :not-established (:physical-frame-preservation safe)))
    (is (= :not-established (:universal-physical-refinement safe)))
    (is (= :not-issued (:certificate safe)))
    (is (= :not-admitted (:admission safe)))
    (is (= :none (:transaction-authority safe)))
    (is (= :none (:retry-authority safe)))
    (is (= :none (:settlement-authority safe)))
    (is (= :none (:authority safe)))
    (is (not-any? #(contains? safe %)
                  [:node :projection-source :transaction-key :physical-acquisition
                   :snapshot-system-time :external-read-basis :observed-rows]))))

(deftest committed-replacement-with-same-pinned-ledger
  (let [id (fresh-id) s (source id) before (document id)
        _ (put! before)
        changed (assoc before :revision 1 :value "private-replaced-v1522")
        key (put! changed)
        {:keys [report handle]} (observe! s key)]
    (is (= :bounded-poststate-and-committed-ledger-agreement (:status report)))
    (is (= :snapshot-not-earlier-than-transaction (:basis-ordering report)))
    (is (true? (:poststate-read-basis-joined-to-ledger? report)))
    (is (every? true? (vals (:ledger-checks report))))
    (is (= changed (get-in report [:physical-acquisition :observed-rows 0 :document])))
    (is (string? (get-in report [:physical-acquisition :external-read-basis
                                :snapshot-token])))
    (bounded-only report handle)))

(deftest deleting-declared-mapped-document-fails-closed
  (let [id (fresh-id) s (source id)
        _ (put! (document id))
        key (delete! id)]
    ;; Deletion is a real committed transaction, but the acquisition cannot
    ;; map the declared abstract fields from an absent physical document.
    (is (= [:domain :missing-mapped-document]
           (rejected #(observe! s key))))))

(deftest recreation-after-delete-can-match-an-unrelated-key
  (let [id (fresh-id) s (source id) doc (document id)
        _ (put! doc)
        deletion-key (delete! id)
        _ (put! (assoc doc :revision 2))
        {:keys [report handle]} (observe! s deletion-key)]
    ;; Reuse of the DELETE key cannot establish that the deleted document
    ;; caused the recreated poststate; the finite ledger join can still pass.
    (is (= :bounded-poststate-and-committed-ledger-agreement (:status report)))
    (is (= 2 (get-in report [:physical-acquisition :observed-rows 0 :document :revision])))
    (bounded-only report handle)))

(deftest missing-declared-physical-field-fails-even-with-real-ledger
  (let [id (fresh-id) s (source id) doc (document id)
        _ (put! doc) key (put! (dissoc doc :value))]
    (is (= [:domain :missing-mapped-physical-field]
           (rejected #(observe! s key))))))

(deftest newly-added-field-to-whole-document-fails
  (let [id (fresh-id) s (source id) doc (document id)
        _ (put! doc) key (put! (assoc doc :injected "private-extra-v1522"))]
    (is (= [:domain :unexpected-protected-physical-field]
           (rejected #(observe! s key))))))

(deftest replacement-with-different-mapped-value-remains-only-a-diagnostic
  (let [id (fresh-id) s (source id) doc (document id)
        _ (put! doc)
        key (put! (assoc doc :value "private-value-from-foreign-writer"))
        {:keys [report handle]} (observe! s key)]
    (is (= :bounded-poststate-and-committed-ledger-agreement (:status report)))
    (is (= "private-value-from-foreign-writer"
           (get-in report [:physical-acquisition :observed-rows 0 :document :value])))
    (bounded-only report handle)))

(deftest foreign-transaction-on-other-document-also-matches
  (let [id (fresh-id) other (fresh-id) doc (document id) s (source id)
        _ (put! doc)
        key (put! {:xt/id other :untracked "private-other-writer"})
        {:keys [report handle]} (observe! s key)]
    (is (= :bounded-poststate-and-committed-ledger-agreement (:status report)))
    (is (= doc (get-in report [:physical-acquisition :observed-rows 0 :document])))
    (bounded-only report handle)))

(deftest earlier-key-does-not-identify-later-writer
  (let [id (fresh-id) s (source id) doc (document id)
        earlier (put! doc)
        _ (put! (assoc doc :revision 11))
        {:keys [report handle]} (observe! s earlier)]
    (is (= :bounded-poststate-and-committed-ledger-agreement (:status report)))
    (is (= 11 (get-in report [:physical-acquisition :observed-rows 0 :document :revision])))
    (bounded-only report handle)))

(deftest frame-only-deletion-and-recreation-are-not-attributed-to-key
  (let [id (fresh-id) frame-id (fresh-id)
        s (with-frame-only (source id) frame-id #{:frame-note})
        _ (put! (document id))
        _ (put! {:xt/id frame-id :free "private-unprotected"})
        deleted (delete! frame-id)
        {:keys [report handle]} (observe! s deleted)]
    (is (= :absent
           (get-in report [:physical-acquisition :observed-rows 1 :observed-document-status])))
    (bounded-only report handle)
    (let [recreated (put! {:xt/id frame-id :free "private-recreated"})
          {:keys [report handle]} (observe! s recreated)]
      (is (= :present
             (get-in report [:physical-acquisition :observed-rows 1 :observed-document-status])))
      (bounded-only report handle))))

(deftest protected-frame-only-field-remains-unmappable-without-a-declaration
  (let [id (fresh-id) frame-id (fresh-id)
        s (with-frame-only (source id) frame-id #{:frame-note})
        _ (put! (document id))
        key (put! {:xt/id frame-id :frame-note "private-protected"})]
    (is (= [:domain :unexpected-protected-physical-field]
           (rejected #(observe! s key))))))

(deftest undeclared-document-changes-do-not-expand-the-observed-domain
  (let [id (fresh-id) unknown (fresh-id) s (source id)
        _ (put! (document id))
        key (put! {:xt/id unknown :outside "private-outside"})
        {:keys [report handle]} (observe! s key)]
    (is (= 1 (get-in report [:physical-acquisition :observed-protected-target-count])))
    (is (= :bounded-poststate-and-committed-ledger-agreement (:status report)))
    (bounded-only report handle)))

(deftest foreign-node-key-is-not-joined
  (let [id (fresh-id) s (source id) _ (put! (document id))]
    (with-open [other (xtn/start-node {})]
      (let [foreign-key (xt/execute-tx other [[:put-docs table (document (fresh-id))]])]
        (is (= [:poststate :no-matching-committed-ledger-row]
               (rejected #(observe! s foreign-key))))))))

(deftest incomplete-and-malformed-keys-are-rejected
  (let [id (fresh-id) s (source id) _ (put! (document id))]
    (doseq [key [nil {} {:tx-id 1} "not-a-key" 4]]
      (is (= [:poststate :no-complete-public-transaction-key]
             (rejected #(observe! s key)))))))

(deftest source-substitution-fails-and-no-issuing-from-summary
  (let [id (fresh-id) s (source id) key (put! (document id))
        {:keys [report handle]} (observe! s key)
        other (source (fresh-id))
        safe (post/redacted-summary report handle)]
    (is (= [:poststate :stale-or-substituted-poststate-report]
           (rejected #(post/require-live-poststate-basis!
                        (assoc report :projection-source other) handle))))
    (is (= [:poststate :stale-or-substituted-poststate-report]
           (rejected #(post/require-live-poststate-basis!
                        (assoc report :certificate :issued) handle))))
    (is (= [:poststate :invalid-process-local-handle]
           (rejected #(post/require-live-poststate-basis! report (Object.)))))
    (is (= :not-issued (:certificate safe)))
    (is (= :none (:authority safe)))
    (is (= :not-established (:physical-poststate-transaction-causality safe)))))

(deftest reports-and-process-local-handles-are-not-interchangeable
  (let [id (fresh-id) s (source id) key (put! (document id))
        {r1 :report h1 :handle} (observe! s key)
        {r2 :report h2 :handle} (observe! s key)]
    (is (not (identical? r1 r2)))
    (is (= [:poststate :stale-or-substituted-poststate-report]
           (rejected #(post/require-live-poststate-basis! r1 h2))))
    (is (= [:poststate :stale-or-substituted-poststate-report]
           (rejected #(post/require-live-poststate-basis! r2 h1))))
    (is (identical? r1 (post/require-live-poststate-basis! r1 h1)))
    (is (identical? r2 (post/require-live-poststate-basis! r2 h2)))))

(deftest read-only-join-cannot-submit-or-retry-transactions
  (let [id (fresh-id) s (source id) key (put! (document id))]
    (is (= :bounded-poststate-and-committed-ledger-agreement
           (with-redefs [xt/execute-tx (fn [& _]
                                         (throw (ex-info "unexpected write" {})))
                         xt/submit-tx (fn [& _]
                                        (throw (ex-info "unexpected submit" {})))]
             (get-in (observe! s key) [:report :status]))))))

(deftest substituted-read-and-capability-seams-rejected
  (let [id (fresh-id) s (source id) key (put! (document id))]
    (is (= [:poststate :substituted-acquisition-or-xtdb-interface]
           (rejected #(with-redefs [xt/q (fn [& _] nil)]
                        (observe! s key)))))
    (is (= [:poststate :substituted-acquisition-or-xtdb-interface]
           (rejected #(with-redefs [acquisition/acquire-physical-domain!
                                    (fn [& _] nil)]
                        (observe! s key)))))
    (is (= [:poststate :substituted-acquisition-or-xtdb-interface]
           (rejected #(with-redefs [basis/<-time-basis-str (fn [& _] {})]
                        (observe! s key)))))
    (is (= [:poststate :substituted-acquisition-or-xtdb-interface]
           (rejected #(with-redefs [cap/capability-report! (fn [] {})]
                        (observe! s key)))))))

(deftest redacted-summary-excludes-all-physical-data
  (let [id (fresh-id) doc (document id) s (source id)
        key (put! doc) {:keys [report handle]} (observe! s key)
        safe (post/redacted-summary report handle)
        rendered (pr-str safe)]
    (is (not-any? #(contains? safe %) [:node :projection-source
                                     :transaction-key :physical-acquisition
                                     :snapshot-system-time]))
    (doseq [secret [id "private-created-v1522" "private-unchanged-v1522"
                    "private-note-v1522" "snapshot-token" "xt.txs"]]
      (is (not (str/includes? rendered secret))))
    (bounded-only report handle)))

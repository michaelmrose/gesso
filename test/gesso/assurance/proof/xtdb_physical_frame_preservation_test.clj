(ns gesso.assurance.proof.xtdb-physical-frame-preservation-test
  "v1520: Permanent adversarial tests of v1519's supplied external-frame
   comparison. Exercise actual pinned XTDB acquisitions and ensure bounded
   observation is never confused with authenticated execution or writer
   completeness. Independent tests use distinct document identifiers."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [clojure.string :as str]
            [gesso.assurance.proof.xtdb-physical-frame-preservation :as frame]
            [gesso.assurance.proof.physical-domain-acquisition :as acquire]
            [gesso.assurance.proof.physical-domain-projection :as projection]
            [gesso.assurance.proof.xtdb-write-footprint-accounting :as footprint]
            [gesso.model.command :as command]
            [gesso.model.tx :as tx]
            [xtdb.api :as xt]
            [xtdb.node :as xtn]))

(def ^:private table :gesso_v1520_frame_probe)
(def ^:private versions {:revision-key :revision
                         :created-at-key :created-at
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
(defn- with-node [f]
  (with-open [n (xtn/start-node {})]
    (reset! !node n)
    (try (f) (finally (reset! !node nil)))))
(use-fixtures :once with-node)

(defn- node []
  (or @!node (throw (ex-info "Missing XTDB test fixture" {}))))
(defn- fresh-id [] (str "gesso-v1520-" (random-uuid)))
(defn- document [id]
  {:xt/id id :revision 0
   :created-at "PRIVATE-created-1520"
   :updated-at "PRIVATE-unchanged-1520"
   :value "PRIVATE-note-1520"})
(defn- source
  ([id] (source id nil :all))
  ([id secondary-id mode]
   {:obligation-source obligation
    :protected-domain-source
    {:plan {:commands [(command/create table
                              (cond-> (document id)
                                secondary-id (dissoc :value))
                              versions)]
            :emit false}
     :protected-documents
     (cond-> [{:target [table id] :fields :all}]
       secondary-id (conj {:target [table secondary-id] :fields mode}))}
    :field-locations
    {:request/status {:target [table id] :physical-field :revision}
     :request/owner {:target [table id] :physical-field :created-at}
     :request/version {:target [table id] :physical-field :updated-at}
     :request/note (if secondary-id
                     {:target [table secondary-id] :physical-field :frame-note}
                     {:target [table id] :physical-field :value})}}))
(defn- with-unmapped-frame [s secondary-id mode]
  (update-in s [:protected-domain-source :protected-documents]
             conj {:target [table secondary-id] :fields mode}))
(defn- put! [doc]
  (xt/execute-tx (node) [[:put-docs table doc]]))
(defn- delete! [id]
  (xt/execute-tx (node) [[:delete-docs table id]]))
(defn- acquire! [s] (acquire/acquire-physical-domain! (node) s))
(defn- input [s before after]
  {:projection-source s :pre-acquisition before :post-acquisition after})
(defn- compare! [s before after]
  (frame/compare-external-physical-frame! (input s before after)))
(defn- reason [f]
  (try (f) :accepted
       (catch clojure.lang.ExceptionInfo e
         (if (:xtdb-physical-frame-preservation/rejection (ex-data e))
           (:reason (ex-data e))
           (throw e)))))
(defn- frame-limits [r]
  (is (= :bounded-supplied-external-frame-diagnostic-only (:proof-status r)))
  (is (= :not-authenticated (:external-observation-order r)))
  (is (= :caller-supplied-unattested (:pre-acquisition-origin r)))
  (is (= :caller-supplied-unattested (:post-acquisition-origin r)))
  (is (= :not-authenticated (:transaction-internal-prestate r)))
  (is (= :not-authenticated (:actual-submitted-operations r)))
  (is (= :not-established (:complete-physical-writer-coverage r)))
  (is (= :not-established (:frame-outside-enumerated-domain r)))
  (is (= :not-established (:actual-physical-frame-preservation r)))
  (is (= :not-established (:universal-refinement r)))
  (is (= :not-issued (:certificate r)))
  (is (= :not-admitted (:admission r)))
  (is (= :none (:authority r)))
  (is (= :none (:transaction-authority r)))
  (is (= :none (:retry-authority r)))
  (is (= :none (:settlement-authority r))))

(deftest preserved-framed-fields-with-changed-business-fields
  (let [id (fresh-id) doc (document id) s (source id)]
    (put! doc)
    (let [before (acquire! s)]
      (put! (assoc doc :revision 1 :created-at "PRIVATE-owner-1520"))
      (let [after (acquire! s) r (compare! s before after)]
        (is (= :bounded-supplied-frame-agrees (:status r)))
        (is (true? (:finite-supplied-frame-agrees? r)))
        (is (= 2 (:declared-typed-frame-field-count r)))
        (is (empty? (:changed-typed-frame-field-indices r)))
        (is (= r (frame/require-current-external-physical-frame!
                  (input s before after) r)))
        (frame-limits r)))))

(deftest changed-framed-value-diverges
  (let [id (fresh-id) doc (document id) s (source id)]
    (put! doc)
    (let [before (acquire! s)]
      (put! (assoc doc :value "PRIVATE-different-1520"))
      (let [r (compare! s before (acquire! s))]
        (is (= :bounded-supplied-frame-diverges (:status r)))
        (is (= [0] (:changed-typed-frame-field-indices r)))
        (is (false? (:finite-supplied-frame-agrees? r)))
        (frame-limits r)))))

(deftest changing-second-mapped-frame-field-diverges
  (let [id (fresh-id) doc (document id) s (source id)]
    (put! doc)
    (let [before (acquire! s)]
      (put! (assoc doc :updated-at "PRIVATE-different-1520"))
      (let [r (compare! s before (acquire! s))]
        (is (= :bounded-supplied-frame-diverges (:status r)))
        (is (= [1] (:changed-typed-frame-field-indices r)))))))

(deftest multiple-framed-changes-report-both-indices
  (let [id (fresh-id) doc (document id) s (source id)]
    (put! doc)
    (let [before (acquire! s)]
      (put! (assoc doc :value "PRIVATE-different-1520"
                       :updated-at "PRIVATE-different-1520"))
      (let [r (compare! s before (acquire! s))]
        (is (= [0 1] (:changed-typed-frame-field-indices r)))
        (is (= :bounded-supplied-frame-diverges (:status r)))))))

(deftest whole-document-frame-only-change-diverges
  (let [id (fresh-id) other (fresh-id)
        s (source id other :all)
        secondary {:xt/id other :frame-note "PRIVATE-original-1520"}]
    (put! (dissoc (document id) :value))
    (put! secondary)
    (let [before (acquire! s)]
      (put! (assoc secondary :frame-note "PRIVATE-changed-1520"))
      (let [r (compare! s before (acquire! s))]
        (is (= :bounded-supplied-frame-diverges (:status r)))
        (is (= [0] (:changed-frame-only-document-indices r)))
        (is (= [0] (:changed-typed-frame-field-indices r)))
        (is (zero? (:partially-protected-frame-only-document-count r)))
        (frame-limits r)))))

(deftest whole-document-frame-only-unexpected-field-fails-acquisition
  (let [id (fresh-id) other (fresh-id)
        s (source id other :all)
        secondary {:xt/id other :frame-note "PRIVATE-original-1520"}]
    (put! (dissoc (document id) :value))
    (put! secondary)
    (let [before (acquire! s)]
      (put! (assoc secondary :injected "PRIVATE-extra-1520"))
      ;; The v1492 whole-document acquisition refuses to accept an extra
      ;; protected field; v1519 cannot legitimately compare that row.
      (is (thrown? clojure.lang.ExceptionInfo (acquire! s)))
      (is (= :bounded-supplied-frame-agrees
             (:status (compare! s before before)))))))

(deftest partial-frame-only-ignores-unprotected-value-change
  (let [id (fresh-id) other (fresh-id)
        s (source id other #{:frame-note})
        secondary {:xt/id other :frame-note "PRIVATE-guarded-1520"
                   :free "PRIVATE-initial-1520"}]
    (put! (dissoc (document id) :value))
    (put! secondary)
    (let [before (acquire! s)]
      (put! (assoc secondary :free "PRIVATE-changed-1520"
                             :new-free "PRIVATE-extra-1520"))
      (let [r (compare! s before (acquire! s))]
        (is (= :bounded-supplied-frame-agrees (:status r)))
        (is (empty? (:changed-frame-only-document-indices r)))
        (is (= 1 (:partially-protected-frame-only-document-count r)))
        (is (pos? (:unprotected-observed-field-count-after r)))
        (frame-limits r)))))

(deftest partial-frame-only-catches-protected-value-change
  (let [id (fresh-id) other (fresh-id)
        s (source id other #{:frame-note})
        secondary {:xt/id other :frame-note "PRIVATE-initial-1520"
                   :free "PRIVATE-initial-1520"}]
    (put! (dissoc (document id) :value))
    (put! secondary)
    (let [before (acquire! s)]
      (put! (assoc secondary :frame-note "PRIVATE-changed-1520"))
      (let [r (compare! s before (acquire! s))]
        (is (= [0] (:changed-frame-only-document-indices r)))
        (is (= :bounded-supplied-frame-diverges (:status r)))))))

(deftest absent-to-present-frame-only-is-a-divergence
  (let [id (fresh-id) other (fresh-id)
        s (with-unmapped-frame (source id) other #{:frame-note})]
    (put! (document id))
    (let [before (acquire! s)]
      (put! {:xt/id other :free "PRIVATE-not-protected-1520"})
      (let [r (compare! s before (acquire! s))]
        (is (= [0] (:changed-frame-only-document-indices r)))
        (is (= :bounded-supplied-frame-diverges (:status r)))))))

(deftest present-to-absent-frame-only-is-a-divergence
  (let [id (fresh-id) other (fresh-id)
        s (with-unmapped-frame (source id) other #{:frame-note})]
    (put! (document id))
    (put! {:xt/id other :free "PRIVATE-not-protected-1520"})
    (let [before (acquire! s)]
      (delete! other)
      (let [r (compare! s before (acquire! s))]
        (is (= [0] (:changed-frame-only-document-indices r)))
        (is (= :bounded-supplied-frame-diverges (:status r)))))))

(deftest unchanged-absence-does-not-become-fabricated-change
  (let [id (fresh-id) other (fresh-id)
        s (with-unmapped-frame (source id) other #{:frame-note})]
    (put! (document id))
    (let [before (acquire! s) after (acquire! s)
          r (compare! s before after)]
      (is (= :bounded-supplied-frame-agrees (:status r)))
      (is (= 1 (:declared-frame-only-document-count r)))
      (is (empty? (:changed-frame-only-document-indices r))))))

(deftest missing-mapped-field-fails-before-frame-comparison
  (let [id (fresh-id) s (source id) doc (document id)]
    (put! doc)
    (let [before (acquire! s)]
      (put! (dissoc doc :value))
      (is (thrown? clojure.lang.ExceptionInfo (acquire! s)))
      (is (= :bounded-supplied-frame-agrees
             (:status (compare! s before before)))))))

(deftest unexpected-field-on-written-whole-document-fails-acquisition
  (let [id (fresh-id) s (source id) doc (document id)]
    (put! doc)
    (let [before (acquire! s)]
      (put! (assoc doc :injected "PRIVATE-extra-1520"))
      (is (thrown? clojure.lang.ExceptionInfo (acquire! s)))
      (is (= :bounded-supplied-frame-agrees
             (:status (compare! s before before)))))))

(deftest source-change-invalidates-comparison
  (let [id (fresh-id) s (source id)]
    (put! (document id))
    (let [before (acquire! s) after (acquire! s)]
      (is (not= :accepted
                (reason #(compare! (update s :field-locations dissoc
                                          :request/note) before after))))
      (is (not= :accepted
                (reason #(compare! (assoc-in s [:field-locations
                                                :request/note :physical-field]
                                             :revision) before after))))
      (is (not= :accepted
                (reason #(compare! (assoc-in s [:protected-domain-source
                                                :protected-documents 0 :fields]
                                             #{:revision}) before after)))))))

(deftest cross-source-acquisitions-cannot-be-mixed
  (let [id (fresh-id) other (fresh-id) s (source id) t (source other)]
    (put! (document id))
    (put! (document other))
    (let [before (acquire! s) after (acquire! t)]
      (is (= :invalid-or-unbound-acquisition
             (reason #(compare! s before after)))))))

(deftest malformed-or-forged-acquisition-reports-fail-closed
  (let [id (fresh-id) s (source id)]
    (put! (document id))
    (let [before (acquire! s) after (acquire! s)]
      (doseq [tampered [(assoc before :certificate :issued)
                        (assoc before :read-basis-authenticity :proved)
                        (dissoc before :observed-rows)
                        (assoc before :surprise :extra)
                        (assoc before :observed-rows [])
                        (assoc-in before [:observed-rows 0 :protection-mode]
                                  :declared-fields)
                        (assoc-in before [:observed-rows 0 :mapped-field-count]
                                  999)
                        (assoc-in before [:external-read-basis :current-time]
                                  "fake-time")]]
        (is (not= :accepted (reason #(compare! s tampered after)))))
      (is (= :invalid-map-shape
             (reason #(frame/compare-external-physical-frame!
                       (assoc (input s before after) :certificate :issued)))))
      (is (= :invalid-map-shape
             (reason #(compare! s (with-meta before {:pretend :issued}) after)))))))

(deftest inconsistent-row-and-aggregate-values-rejected
  (let [id (fresh-id) s (source id)]
    (put! (document id))
    (let [before (acquire! s) after (acquire! s)]
      (is (= :divergent-or-incomplete-acquisition-row
             (reason #(compare! s
                                (assoc-in before [:observed-rows 0 :mapped-values]
                                          {:request/note "PRIVATE-forged-1520"})
                                after))))
      (is (= :inconsistent-acquisition-summary
             (reason #(compare! s
                                (assoc before :abstract-physical-values {})
                                after))))
      (is (= :invalid-or-incomplete-acquisition-rows
             (reason #(compare! s (assoc before :observed-rows []) after)))))))

(deftest altered-or-elevated-comparison-report-cannot-be-rechecked
  (let [id (fresh-id) s (source id)]
    (put! (document id))
    (let [before (acquire! s) after (acquire! s)
          i (input s before after)
          r (frame/compare-external-physical-frame! i)]
      (is (= r (frame/require-current-external-physical-frame! i r)))
      (doseq [altered [(assoc r :certificate :issued)
                      (assoc r :actual-physical-frame-preservation :proved)
                      (assoc r :universal-refinement :proved)
                      (assoc r :finite-supplied-frame-agrees? false)
                      (assoc r :changed-typed-frame-field-indices [0])
                      (assoc r :status :universal-proof)
                      (assoc r :source (assoc i :untrusted-extra true))]]
        (is (= :stale-or-forged-frame-report
               (reason #(frame/require-current-external-physical-frame!
                         i altered))))))))

(deftest source-and-acquisition-function-substitution-rejected
  (let [id (fresh-id) s (source id)]
    (put! (document id))
    (let [before (acquire! s) after (acquire! s)]
      (doseq [f [(fn [] (with-redefs [projection/describe-physical-domain-projection!
                                     (fn [_] :counterfeit)]
                             (compare! s before after)))
                 (fn [] (with-redefs [footprint/inspect-footprint!
                                     (fn [_] :counterfeit)]
                             (compare! s before after)))
                 (fn [] (with-redefs [tx/prepare
                                     (fn [& _] :counterfeit)]
                             (compare! s before after)))
                 (fn [] (with-redefs [acquire/redacted-summary
                                     (fn [_] :counterfeit)]
                             (compare! s before after)))]]
        (is (= :substituted-source-or-acquisition-checker (reason f)))))))

(deftest no-physical-reads-writes-or-retries-during-comparison
  (let [id (fresh-id) s (source id)]
    (put! (document id))
    (let [before (acquire! s) after (acquire! s)]
      (is (= :bounded-supplied-frame-agrees
             (:status (with-redefs [xt/q (fn [& _]
                                                (throw (Exception. "unexpected read")))
                                   xt/execute-tx (fn [& _]
                                                   (throw (Exception. "unexpected write")))]
                        (compare! s before after)))))
      (is (= :bounded-supplied-frame-agrees
             (:status (with-redefs [xt/q (fn [& _]
                                                (throw (Exception. "unexpected read")))
                                   xt/execute-tx (fn [& _]
                                                   (throw (Exception. "unexpected write")))]
                        (frame/require-current-external-physical-frame!
                         (input s before after)
                         (compare! s before after)))))))))

(deftest undeclared-writer-does-not-create-writer-completeness
  (let [id (fresh-id) other (fresh-id) s (source id)]
    (put! (document id))
    (let [before (acquire! s)]
      (put! {:xt/id other :value "PRIVATE-rogue-1520"})
      (let [r (compare! s before (acquire! s))]
        (is (= :bounded-supplied-frame-agrees (:status r)))
        (is (= :not-established (:complete-physical-writer-coverage r)))
        (is (= :not-established (:frame-outside-enumerated-domain r)))
        (frame-limits r)))))

(deftest caller-supplied-snapshot-order-is-not-authenticated
  (let [id (fresh-id) doc (document id) s (source id)]
    (put! doc)
    (let [before (acquire! s)]
      (put! (assoc doc :value "PRIVATE-changed-1520"))
      (let [after (acquire! s)
            forward (compare! s before after)
            reversed (compare! s after before)
            self (compare! s before before)]
        (is (= :bounded-supplied-frame-diverges (:status forward)))
        (is (= :bounded-supplied-frame-diverges (:status reversed)))
        (is (= :bounded-supplied-frame-agrees (:status self)))
        (is (true? (:same-reported-snapshot-token? self)))
        (is (= :not-authenticated (:external-observation-order reversed)))
        (frame-limits self)))))

(deftest redacted-diagnostics-never-include-rows-or-private-values
  (let [id (fresh-id) doc (document id) s (source id)]
    (put! doc)
    (let [before (acquire! s)]
      (put! (assoc doc :value "PRIVATE-secret-final-1520"))
      (let [r (compare! s before (acquire! s))
            summary (frame/redacted-summary r)
            printed (pr-str summary)]
        (is (not (contains? summary :source)))
        (is (= :bounded-supplied-frame-diverges (:status summary)))
        (is (not (re-find #"PRIVATE|gesso-v1520-|:observed-rows|:field-locations"
                          printed)))
        (is (not (str/includes? printed
                                 (get-in before [:external-read-basis
                                                 :snapshot-token]))))
        (is (= (:changed-typed-frame-field-indices r)
               (:changed-typed-frame-field-indices summary)))
        (frame-limits r)))))

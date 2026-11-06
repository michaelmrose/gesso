(ns gesso.assurance.proof.physical-domain-acquisition-test
  "v1493: Permanent adversarial tests for v1492 finite external XTDB domain
   acquisition. These use genuine embedded XTDB reads and the original v1491
   source declarations. A passing observation is NOT a certified physical
   projection, an authenticated DML internal prestate, an exhaustive domain,
   a complete writer inventory, or an authority to execute or settle."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [gesso.assurance.proof.physical-domain-acquisition :as subject]
            [gesso.assurance.proof.physical-domain-projection :as projection]
            [gesso.model.command :as command]
            [xtdb.api :as xt]
            [xtdb.basis :as xt.basis]
            [xtdb.node :as xtn]))

(def ^:private table :gesso_v1493_domain_probe)
(def ^:private version-keys
  {:revision-key :revision :created-at-key :created-at
   :updated-at-key :updated-at})
(def ^:private assertion
  {:subject {:kind :operation :id :example/claim
             :interface {:operation :example/claim}}
   :state-types
   {:request/status {:type :enum :name :request/status
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
(def ^:private obligation-source
  {:construction assertion
   :initial-condition {:readers #{:request/status}
                       :expression [:= [:read :request/status] [:literal :open]]}
   :invariants
   {:request/status-valid
    {:readers #{:request/status}
     :expression [:in [:read :request/status]
                  [:literal #{:open :claimed :done}]]}
    :request/version-nonnegative
    {:readers #{:request/version}
     :expression [:>= [:read :request/version] [:literal 0]]}}})

(defonce ^:private !node (atom nil))
(defn- with-node [f]
  (with-open [node (xtn/start-node {})]
    (reset! !node node)
    (try (f) (finally (reset! !node nil)))))
(use-fixtures :once with-node)
(defn- node []
  (or @!node (throw (ex-info "Missing v1493 test node" {}))))
(defn- fresh-id [label] (str "v1493-" label "-" (random-uuid)))
(defn- document [id]
  {:xt/id id :revision 0 :created-at "created-SECRET1493"
   :updated-at "updated-SECRET1493" :value "value-SECRET1493"})
(defn- source [id]
  {:obligation-source obligation-source
   :protected-domain-source
   {:plan {:commands [(command/create table (document id) version-keys)]
           :emit false}
    :protected-documents [{:target [table id] :fields :all}]}
   :field-locations
   {:request/status {:target [table id] :physical-field :revision}
    :request/owner {:target [table id] :physical-field :created-at}
    :request/version {:target [table id] :physical-field :updated-at}
    :request/note {:target [table id] :physical-field :value}}})
(defn- put! [doc]
  (xt/execute-tx (node) [[:put-docs table doc]]))
(defn- reason [f]
  (try (f) :accepted
       (catch clojure.lang.ExceptionInfo ex
         (let [data (ex-data ex)]
           (is (not (re-find #"SECRET1493" (pr-str data)))
               "rejection metadata must not reveal observed content")
           (:reason data)))))
(defn- acquire [s] (subject/acquire-physical-domain! (node) s))
(defn- no-assurance [r]
  (is (= :bounded-pinned-external-field-observation (:status r)))
  (is (= :external-read-diagnostic-only (:proof-status r)))
  (is (= :caller-supplied-unattested (:read-node-provenance r)))
  (is (= :not-established (:read-basis-authenticity r)))
  (is (= :not-established (:physical-value-and-type-translation r)))
  (is (= :not-authenticated (:transaction-internal-prestate r)))
  (is (= :not-established (:complete-writer-coverage r)))
  (is (= :not-established (:physical-frame-preservation r)))
  (is (= :not-established (:universal-physical-refinement r)))
  (is (= :not-issued (:certificate r)))
  (is (= :not-admitted (:admission r)))
  (is (= :none (:transaction-authority r)))
  (is (= :none (:retry-authority r)))
  (is (= :none (:settlement-authority r)))
  (is (= :none (:authority r))))

(deftest complete-whole-document-observation-is-only-conditional
  (let [id (fresh-id "complete") doc (document id) s (source id)]
    (put! doc)
    (let [r (acquire s)]
      (is (= 1 (:observed-protected-target-count r)))
      (is (= 4 (:observed-mapped-field-count r)))
      (is (= 0 (:observed-unprotected-field-count r)))
      (is (= (dissoc doc :xt/id)
             (dissoc (get-in r [:observed-rows 0 :document]) :xt/id)))
      (is (= "value-SECRET1493" (get-in r [:abstract-physical-values :request/note])))
      (is (= #{:snapshot-token :current-time}
             (set (keys (:external-read-basis r)))))
      (no-assurance r))))

(deftest missing-mapped-document-fails-closed
  (let [id (fresh-id "missing-document")]
    (is (= :missing-mapped-document (reason #(acquire (source id)))))))

(deftest missing-required-field-fails-closed
  (let [id (fresh-id "missing-field")]
    (put! (dissoc (document id) :value))
    (is (= :missing-mapped-physical-field
           (reason #(acquire (source id)))))))

(deftest extra-field-in-whole-document-fails-closed
  (let [id (fresh-id "extra-field")]
    (put! (assoc (document id) :undeclared-value "SECRET1493-EXTRA"))
    (is (= :unexpected-protected-physical-field
           (reason #(acquire (source id)))))))

(deftest physically-typed-values-are-not-silently-certified
  (let [id (fresh-id "wrong-abstract-type") s (source id)]
    ;; The physical :revision is an integer despite its mapped enum type;
    ;; Gesso observes it but MUST NOT claim to have verified type translation.
    (put! (document id))
    (let [r (acquire s)]
      (is (= 0 (get-in r [:abstract-physical-values :request/status])))
      (is (= :not-established (:physical-value-and-type-translation r)))
      (is (= :not-issued (:certificate r))))))

(deftest partial-protection-reports-instead-of-certifying-unprotected-values
  (let [id (fresh-id "partial-main") other (fresh-id "partial-secondary")
        s (source id)
        s2 (-> s
               (assoc-in [:protected-domain-source :protected-documents]
                         [{:target [table id] :fields :all}
                          {:target [table other] :fields #{:note}}])
               (assoc-in [:field-locations :request/note]
                         {:target [table other] :physical-field :note}))]
    (put! (dissoc (document id) :value))
    (put! {:xt/id other :note "SECRET1493-NOTE" :free "outside-protection"})
    (let [r (acquire s2)]
      (is (= 2 (:observed-protected-target-count r)))
      (is (= 1 (:observed-unprotected-field-count r)))
      (is (= "SECRET1493-NOTE" (:request/note (:abstract-physical-values r))))
      (is (= :declared-fields (get-in r [:observed-rows 1 :protection-mode])))
      (no-assurance r))))

(deftest unexpected-protected-field-on-partially-protected-target-fails
  (let [id (fresh-id "partial-unexpected") other (fresh-id "extra-protected")
        s (-> (source id)
              (assoc-in [:protected-domain-source :protected-documents]
                        [{:target [table id] :fields :all}
                         {:target [table other] :fields #{:note :unmapped}}])
              (assoc-in [:field-locations :request/note]
                        {:target [table other] :physical-field :note}))]
    (put! (dissoc (document id) :value))
    (put! {:xt/id other :note "SECRET1493-NOTE" :unmapped "SECRET1493"})
    (is (= :unexpected-protected-physical-field (reason #(acquire s))))))

(deftest missing-protected-frame-only-document-stays-visible
  (let [id (fresh-id "frame-main") absent (fresh-id "absent-frame")
        s (update-in (source id) [:protected-domain-source :protected-documents]
                     conj {:target [table absent] :fields #{:frame-note}})]
    (put! (document id))
    (let [r (acquire s)]
      (is (= 2 (:observed-protected-target-count r)))
      (is (= 1 (:observed-absent-unmapped-target-count r)))
      (is (= #{[table absent]} (:unmapped-protected-targets r)))
      (no-assurance r))))

(deftest undeclared-physical-documents-are-outside-acquisition-scope
  (let [id (fresh-id "declared") rogue (fresh-id "undeclared") s (source id)]
    (put! (document id))
    (put! (assoc (document rogue) :unregistered-write true))
    (let [r (acquire s)]
      (is (= 1 (:observed-protected-target-count r)))
      (is (= #{[table id]} (set (map :target (:observed-rows r)))))
      (is (= :not-established (:complete-writer-coverage r)))
      (is (= :not-established (:universal-physical-refinement r))))))

(deftest one-pinned-external-snapshot-remains-readable-after-new-write
  (let [id (fresh-id "basis") s (source id) before (document id)
        sql "SELECT * FROM gesso_v1493_domain_probe WHERE _id = ? LIMIT 2"]
    (put! before)
    (let [r (acquire s)
          after (assoc before :value "SECRET1493-CHANGED")]
      (put! after)
      (is (= after (first (xt/q (node) [sql id]))))
      (is (= before (first (xt/q (node) [sql id] (:external-read-basis r)))))
      (is (= "value-SECRET1493" (:request/note (:abstract-physical-values r))))
      (is (= :not-authenticated (:transaction-internal-prestate r))))))

(deftest changed-source-and-missing-or-aliased-locations-rejected
  (let [id (fresh-id "stale-source") s (source id)]
    (put! (document id))
    (is (= :invalid-or-stale-physical-projection
           (reason #(acquire (update s :field-locations dissoc :request/note)))))
    (is (= :invalid-or-stale-physical-projection
           (reason #(acquire (assoc-in s [:field-locations :request/note :physical-field]
                                      :revision)))))
    (is (= :invalid-or-stale-physical-projection
           (reason #(acquire (assoc s :invented-extra "SECRET1493")))))))

(deftest incomplete-physical-field-location-is-rejected-even-when-row-exists
  (let [id (fresh-id "wrong-field") s (source id)]
    (put! (document id))
    (is (= :missing-mapped-physical-field
           (reason #(acquire (assoc-in s [:field-locations :request/note :physical-field]
                                      :field-not-present)))))))

(deftest malformed-or-unsupported-targets-never-create-read-authority
  (let [id (fresh-id "targets") s (source id)]
    (put! (document id))
    (is (= :missing-observation-node
           (reason #(subject/acquire-physical-domain! nil s))))
    (is (not= :accepted
              (reason #(acquire (assoc-in s [:field-locations :request/status :target]
                                         [:bad/table id])))))
    (is (not= :accepted
              (reason #(acquire (assoc-in s [:field-locations :request/status :target]
                                         [table {:id id}])))))))

(deftest substituted-read-and-source-seams-rejected
  (let [id (fresh-id "seams") s (source id)]
    (put! (document id))
    (is (= :substituted-source-or-read-seam
           (reason #(with-redefs [xt/q (fn [& _] (throw (ex-info "SECRET1493" {})))]
                      (acquire s)))))
    (is (= :substituted-source-or-read-seam
           (reason #(with-redefs [xt.basis/<-time-basis-str (fn [_] {})]
                      (acquire s)))))
    (is (= :substituted-source-or-read-seam
           (reason #(with-redefs [projection/describe-physical-domain-projection!
                                  (fn [_] (throw (ex-info "SECRET1493" {})))]
                      (acquire s)))))))

(deftest read-only-acquisition-never-submits-physical-transaction
  (let [id (fresh-id "read-only") s (source id)]
    (put! (document id))
    (let [r (with-redefs [xt/execute-tx
                          (fn [& _] (throw (ex-info "Unexpected physical write" {})))]
              (acquire s))]
      (is (= 1 (:observed-protected-target-count r)))
      (is (= :none (:transaction-authority r)))
      (is (= :none (:retry-authority r)))
      (is (= :none (:settlement-authority r))))))

(deftest redacted-summary-never-exposes-values-identifiers-or-basis
  (let [id (fresh-id "redaction") s (source id)]
    (put! (document id))
    (let [r (acquire s)
          summary (subject/redacted-summary r)
          rendered (pr-str summary)]
      (is (not (contains? summary :source)))
      (is (not (contains? summary :observed-rows)))
      (is (not (contains? summary :external-read-basis)))
      (is (not (contains? summary :abstract-physical-values)))
      (is (not (contains? summary :unmapped-protected-targets)))
      (is (not (.contains rendered id)))
      (is (not (.contains rendered "SECRET1493")))
      (is (not (.contains rendered (:snapshot-token (:external-read-basis r)))))
      (is (= :not-established (:read-basis-authenticity summary)))
      (is (= :not-issued (:certificate summary))))))

(deftest edited-report-cannot-be-promoted-to-certified-summary
  (let [id (fresh-id "forgery") s (source id)]
    (put! (document id))
    (let [r (acquire s)]
      (doseq [r2 [(assoc r :certificate :issued)
                  (assoc r :proof-status :proved)
                  (assoc r :authority :execute)
                  (assoc r :type :forged)
                  (assoc r :version -1)]]
        (is (= :invalid-acquisition-report
               (reason #(subject/redacted-summary r2)))))
      ;; Redaction validates only the presentation envelope, not row
      ;; authenticity. It must never be treated as a proof rechecker.
      (is (= :not-authenticated
             (:transaction-internal-prestate (subject/redacted-summary r)))))))

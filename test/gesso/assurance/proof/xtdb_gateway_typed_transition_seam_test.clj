(ns gesso.assurance.proof.xtdb-gateway-typed-transition-seam-test
  "v1755: real-XTDB bounded integration and hostile original-source
   correspondence checks. Reuse the existing independently certified route,
   actual model update and pinned/complete-CAS fixture; do not substitute an
   in-memory fake writer. Source provenance and theorem premises remain open."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [gesso.assurance.proof.xtdb-gateway-typed-transition-seam :as seam]
            [gesso.assurance.proof.transition :as transition]
            [gesso.assurance.proof.xtdb-gateway-plan-bound-guarded-prestate-test]
            [xtdb.api :as xt]))

(def ^:private fixture-ns
  'gesso.assurance.proof.xtdb-gateway-plan-bound-guarded-prestate-test)
(defn- helper [k] (var-get (ns-resolve fixture-ns k)))
(use-fixtures :once (helper 'with-certified-xtdb))

(def ^:private source
  {:subject {:kind :operation :id :example/claim
             :interface {:operation :claim}}
   :state-types {:doc/id {:type :string}
                 :doc/revision {:type :integer}
                 :doc/created {:type :string}
                 :doc/updated {:type :string}
                 :doc/owner {:type :string}
                 :doc/note {:type :string}}
   :input-types {:input/owner {:type :string}}
   :guard {:readers #{:doc/owner}
           :expression [:= [:read :doc/owner] [:literal "available"]]}
   :reads #{:doc/owner :input/owner}
   :writes #{:doc/revision :doc/updated :doc/owner}
   :assignments {:doc/revision [:literal 1]
                 :doc/updated [:literal "after"]
                 :doc/owner [:read :input/owner]}})

(def ^:private mapping
  {:doc/id :xt/id
   :doc/revision :revision
   :doc/created :created-at
   :doc/updated :updated-at
   :doc/owner :owner
   :doc/note :note})

(def ^:private proof-context
  {:invariants
   {:doc/owner-not-empty {:readers #{:doc/owner}
                          :expression [:not= [:read :doc/owner] [:literal ""]]}
    :doc/revision-nonnegative {:readers #{:doc/revision}
                               :expression [:>= [:read :doc/revision] [:literal 0]]}
    :doc/note-not-empty {:readers #{:doc/note}
                         :expression [:not= [:read :doc/note] [:literal ""]]}}
   :read-premises [{:operation-id :example/claim
                    :invariant-id :doc/owner-not-empty
                    :field :doc/owner :reader :input/owner
                    :reader-origin :input}]})

(defn- before [] ((helper 'before)))
(defn- after [b] ((helper 'after) b))
(defn- candidate
  ([b] ((helper 'candidate) b))
  ([b a] ((helper 'candidate) b a)))
(defn- put! [doc] ((helper 'put!) doc))
(defn- physical [] ((helper 'physical)))
(defn- case-context [] (helper '*case*))
(defn- node [] (helper '*node*))

(defn- prepare
  ([candidate-source] (prepare candidate-source source mapping {:input/owner "claimed"} proof-context))
  ([candidate-source declaration field-map inputs]
   (prepare candidate-source declaration field-map inputs proof-context))
  ([candidate-source declaration field-map inputs proof-context]
   (let [{:keys [contract issuance]} (case-context)]
     (seam/prepare-typed-transition-one-shot!
      (node) contract issuance :example/claim
      {:request-method :post :uri "/operations/example/claim"}
      candidate-source declaration inputs field-map proof-context))))

(defn- reason [f]
  (try (f) :no-error
       (catch Throwable e
         (if-let [v (or (:reason (ex-data e))
                        (:xtdb.error/code (ex-data e)))]
           v
           (throw e)))))

(deftest existing-physical-writer-receives-matching-guarded-transition
  (let [b (before)]
    (put! b)
    (let [prepared (prepare (candidate b))
          summary ((:summary! prepared))]
      (is (= #{:type :version :submit! :summary!} (set (keys prepared))))
      (is (= seam/report-type (:type prepared) (:type summary)))
      (is (= :prepared (:status summary)))
      (is (= :checked-per-attempt (:original-and-independent-abstract-relations summary)))
      (is (= :existing-complete-top-level-cas (:transaction-time-guard summary)))
      (is (= :source-relative-values-screened-not-proof-established
             (:source-read-theorem-premises summary)))
      (is (= 1 (:claimed-read-premise-count summary)))
      (is (= :not-established (:original-invariant-source-authenticity summary)))
      (is (= :caller-supplied-not-authenticated (:source-declaration-provenance summary)))
      (is (= :not-issued (:certificate summary)))
      (is (= :not-admitted (:admission summary)))
      (is (= b (physical)) "Preparation does not write")
      (is (some? ((:submit! prepared))))
      (is (= (after b) (physical)))
      (is (= :wrapper-permit-consumed (:status ((:summary! prepared)))))
      (is (= :one-shot-already-attempted (reason (:submit! prepared)))))))

(deftest invalid-source-guards-and-effects-never-expose-a-physical-writer
  (let [b (before)
        src (candidate b)]
    (put! b)
    (is (= :original-and-independent-abstract-relation-not-established
           (reason #(prepare src (assoc-in source [:guard :expression 2 1]
                                           "already-claimed")
                             mapping {:input/owner "claimed"}))))
    (is (= :original-and-independent-abstract-relation-not-established
           (reason #(prepare (candidate b (assoc (after b) :note "altered"))))))
    (is (= :original-and-independent-abstract-relation-not-established
           (reason #(prepare (candidate b (assoc (after b) :owner "forged"))))))
    (is (= :invalid-abstract-transition-values
           (reason #(prepare src source mapping {:input/owner 5}))))
    (is (= :invalid-abstract-transition-values
           (reason #(prepare src source mapping {}))))
    (is (= b (physical)))))

(deftest original-source-derived-premises-cannot-be-omitted-or-forged
  (let [b (before) src (candidate b)]
    (put! b)
    (doseq [[ctx expected]
            [[(dissoc proof-context :read-premises) :invalid-or-absent-read-proof-context]
             [(assoc proof-context :read-premises []) :invalid-or-absent-read-proof-context]
             [(assoc proof-context :read-premises
                     (conj (:read-premises proof-context)
                           (first (:read-premises proof-context))))
              :read-premise-inventory-not-source-derived]
             [(assoc-in proof-context [:read-premises 0 :reader] :input/other)
              :read-premise-inventory-not-source-derived]
             [(assoc-in proof-context [:read-premises 0 :reader-origin] :prestate)
              :read-premise-inventory-not-source-derived]
             [(assoc proof-context :invariants
                     (dissoc (:invariants proof-context) :doc/owner-not-empty))
              :read-premise-inventory-not-source-derived]
             [(assoc-in proof-context [:invariants :doc/owner-not-empty :readers]
                        #{:doc/other})
              :unsupported-read-invariant-declaration]]]
      (is (= expected
             (reason #(prepare src source mapping {:input/owner "claimed"} ctx)))))
    (is (= b (physical)))))

(deftest source-read-value-must-satisfy-original-invariant-not-merely-type
  (let [b (before)]
    (put! b)
    (is (= :source-read-premise-failed
           (reason #(prepare (candidate b (assoc (after b) :owner ""))
                             source mapping {:input/owner ""} proof-context))))
    (is (= :source-prestate-invariant-failed
           (reason #(prepare (candidate (assoc b :note ""))
                             source mapping {:input/owner "claimed"} proof-context))))
    (is (= b (physical)))))

(deftest full-bijective-physical-field-correspondence-required
  (let [b (before)
        src (candidate b)]
    (put! b)
    (doseq [invalid [(dissoc mapping :doc/note)
                     (assoc mapping :doc/note :owner)
                     (assoc mapping :doc/id :unrelated)
                     (assoc mapping :doc/extra :irrelevant)]]
      (is (= :incomplete-or-nonbijective-physical-field-map
             (reason #(prepare src source invalid {:input/owner "claimed"})))))
    (is (= :operation-or-candidate-source-mismatch
           (reason #(prepare src (assoc-in source [:subject :id] :example/other)
                             mapping {:input/owner "claimed"}))))
    (is (= :canonical-command-or-source-interface-mismatch
           (reason #(prepare src (assoc-in source [:subject :interface :operation] :cancel)
                             mapping {:input/owner "claimed"}))))
    (is (= b (physical)))))

(deftest physical-cas-rejects-intervening-write-after-screening
  (let [b (before)]
    (put! b)
    (let [g (prepare (candidate b))
          intervened (assoc b :note "changed-after-external-read")]
      (put! intervened)
      (is (= :xtdb/assert-failed (reason (:submit! g))))
      (is (= intervened (physical)))
      (is (= :wrapper-permit-consumed (:status ((:summary! g)))))
      (is (= :one-shot-already-attempted (reason (:submit! g)))))))

(deftest a-substituted-transition-interpreter-consumes-only-permit
  (let [b (before)]
    (put! b)
    (let [g (prepare (candidate b))]
      (with-redefs [transition/observe-transition! (fn [& _] {:matches? true})]
        (is (= :substituted-source-or-physical-interpreter
               (reason (:submit! g)))))
      (is (= b (physical)))
      (is (= :one-shot-already-attempted (reason (:submit! g)))))))

(deftest physical-and-abstract-scope-remain-distinct
  (let [b (before)]
    (put! b)
    (let [g (prepare (candidate b))
          s ((:summary! g))]
      (is (= :conditional-abstract-and-physical-gateway-join (:proof-status s)))
      (is (= :not-established (:principal-authorization s)))
      (is (= :not-established (:whole-application-writer-coverage s)))
      (is (= :not-established (:physical-proof-refinement s)))
      (is (= :not-established (:commit-causality s)))
      (is (= :not-established (:aba-history-detection s)))
      (is (= :one-shot-submission-capability (:transaction-authority s)))
      (is (not-any? #(contains? s %)
                    [:source :target :input :before :after :node :snapshot-token
                     :gateway :plan :contract :tx-ops])))))

(deftest physical-target-and-pinned-prestate-cannot-be-faked-by-valid-abstract-steps
  (let [b (before)
        unrelated (assoc b :xt/id "v1755-unrelated-document")
        forged-before (assoc b :note "not-the-physical-value")]
    (put! b)
    ;; Each of these documents is a valid abstract typed step. Neither may
    ;; escape the existing physical resolver and acquired-prestate checks.
    (is (= :physical-gateway-preparation-rejected
           (reason #(prepare (candidate unrelated)))))
    (is (= :physical-gateway-preparation-rejected
           (reason #(prepare (candidate forged-before)))))
    (is (= b (physical)))))

(ns gesso.assurance.proof.xtdb-gateway-plan-bound-guarded-prestate-test
  "v1587: permanent real-XTDB adversarial checks for v1586's *opt-in*
   certified-plan target / physical command / independently acquired before
   / complete top-level guarded one-shot submission composition.

   A real certified-plan executor and closed application contract establish
   the exact issued target callback. An embedded XTDB node exercises actual
   writes and transaction-time guard failures. These are bounded regression
   observations, not a certificate of business target authenticity, principal
   permission, hidden transaction-internal prestate, ABA exclusion, commit
   causality, or whole-deployment writer coverage."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [gesso.assurance.proof.xtdb-gateway-operation-target-binding :as target-binding]
            [gesso.assurance.proof.xtdb-gateway-plan-bound-guarded-prestate :as guarded]
            [gesso.assurance.proof.xtdb-gateway-route-target-provenance :as provenance]
            [gesso.live.application-preflight.certified-contract :as certified]
            [gesso.live.application-preflight.fixture :as fixture]
            [gesso.model.certified-plan :as plan]
            [gesso.model.certified-plan-test]
            [gesso.model.command :as command]
            [xtdb.api :as xt]
            [xtdb.node :as xtn]))

(def ^:private operation :example/claim)
(def ^:private uri "/operations/example/claim")
(def ^:private request {:request-method :post :uri uri})
(def ^:private table :requests)
(def ^:private private-id "v1587-private-physical-target")
(def ^:private canary "v1587-secret-must-not-be-disclosed")
(def ^:private version
  {:revision-key :revision :created-at-key :created-at
   :updated-at-key :updated-at})
(def ^:dynamic *case* nil)
(def ^:dynamic *node* nil)

(defn- existing-helper [name]
  (let [v (ns-resolve 'gesso.model.certified-plan-test name)]
    (when-not v
      (throw (ex-info "Missing certified-plan regression helper"
                      {:reason :missing-certified-plan-helper})))
    (var-get v)))

(defn- certified-case []
  (let [target! (provenance/issue-literal-route-plan-target!
                 {operation {:method :post :uri uri
                             :target [table private-id]}})
        source (assoc (:source ((existing-helper 'environment))) :target! target!)
        executor (plan/build-certified-plan-operation! source)
        handler (certified/issue-certified-handler!
                 {:executor executor
                  :current-source source
                  :request->input! (fn [_] ((existing-helper 'invocation) :reconcile))
                  :result->response! (fn [_ result] {:status 200 :body result})})]
    {:target! target! :executor executor :source source :handler handler}))

(defn- with-certified-xtdb [run]
  (let [{:keys [target! source executor handler] :as c} (certified-case)]
    (fixture/with-closed-application
     {:operations {operation (fixture/trusted-operation
                              operation {:published-change-topics #{}})}}
     (fn [{:keys [assembly]}]
       (let [contract {:application-assembly assembly
                       :semantic-route-handlers {operation handler}
                       :certified-operations
                       {operation {:executor executor :current-source source}}}
             issuance (provenance/issue-certified-plan-bound-literal-route-target-resolver!
                       contract {operation target!})]
         (with-open [node (xtn/start-node {})]
           (binding [*case* (assoc c :contract contract :issuance issuance)
                     *node* node]
             (run))))))))

(use-fixtures :once with-certified-xtdb)

(defn- before []
  {:xt/id private-id :revision 0 :created-at "created"
   :updated-at "before" :owner "available" :note canary})

(defn- after [b]
  (-> b (assoc :revision (inc (:revision b))
               :updated-at "after" :owner "claimed")))

(defn- candidate
  ([b] (candidate b (after b)))
  ([b a]
   {:plan {:commands [(command/update-command table :claim b a version)]
           :emit false}
    :protected-documents [{:target [table (:xt/id b)] :fields :all}]}))

(defn- put! [b]
  (xt/execute-tx *node* [[:put-docs table b]]))

(defn- physical []
  (first (xt/q *node* ["SELECT * FROM requests WHERE _id = ?" private-id])))

(defn- prepare!
  ([source]
   (prepare! *node* (:contract *case*) (:issuance *case*)
             operation request source))
  ([node contract issuance selected req source]
   (guarded/prepare-plan-bound-acquired-one-shot!
    node contract issuance selected req source)))

(defn- observe [f]
  (try {:accepted (f)}
       (catch clojure.lang.ExceptionInfo e
         {:message (ex-message e) :data (ex-data e)})))

(defn- reason [f]
  ;; XTDB transaction conflicts may be Errors rather than ExceptionInfo.
  ;; Preserve unexpected exceptions instead of treating them as guard failures.
  (try (f) :no-error
       (catch Throwable e
         (if-let [code (or (:reason (ex-data e))
                           (:xtdb.error/code (ex-data e)))]
           code
           (throw e)))))

(defn- reject! [expected f]
  (let [{:keys [data message] :as result} (observe f)]
    (is (not (contains? result :accepted)) "An invalid preparation must not expose authority")
    (is (= expected (:reason data)) (str "Unexpected rejection: " (pr-str result)))
    (is (= true (:xtdb-gateway-plan-bound-guarded-prestate/rejection data)))
    (is (= #{:xtdb-gateway-plan-bound-guarded-prestate/rejection :reason}
           (set (keys data)))
        "Rejection data must not expose a document, plan, SQL, or receipt")
    (is (= "Unsupported plan-bound guarded XTDB prestate." message))
    (is (not (str/includes? (str message " " (pr-str data)) private-id)))
    (is (not (str/includes? (str message " " (pr-str data)) canary)))))

(defn- check-nonclaims! [r]
  (doseq [[key expected]
          {:external-basis-authenticity :caller-node-unattested
           :transaction-internal-read-basis :not-authenticated
           :aba-history-detection :not-established
           :physical-commit-causality :not-established
           :principal-authorization :not-established
           :complete-deployed-writer-coverage :not-established
           :abstract-operation-refinement :not-established
           :target-table-business-truth :application-premise
           :proof-status :conditional-plan-bound-acquired-prestate
           :certificate :not-issued
           :admission :not-admitted
           :transaction-authority :one-shot-submission-capability}]
    (is (= expected (get r key)) (str "Assurance overstated: " key)))
  (is (= #{:application-literal-target-table-is-correct-and-authorized
           :ring-route-selection-is-authentic
           :supplied-node-is-the-intended-deployment-node
           :transaction-assert-and-put-semantics-remain-valid
           :no-unmediated-writers-guaranteed
           :physical-commit-causality-not-proved}
         (:open-obligations r)))
  (is (not-any? #(contains? r %)
                [:source :contract :resolver :target :node :plan :before :after
                 :transaction-key :tx-key :tx-ops :gateway :snapshot-token]))
  (is (not (str/includes? (pr-str r) private-id)))
  (is (not (str/includes? (pr-str r) canary))))

(deftest real-physical-before-is-required-and-correct-write-commits
  (let [b (before)]
    (put! b)
    (let [g (prepare! (candidate b))
          pre ((:summary! g))]
      (is (= #{:type :version :submit! :summary!} (set (keys g)))
          "Do not leak the underlying submit gateway or unrestricted XTDB writer")
      (is (= guarded/report-type (:type g) (:type pre)))
      (is (= guarded/report-version (:version g) (:version pre)))
      (is (= :prepared (:status pre)))
      (is (= :prepared (:underlying-gateway-status pre)))
      (is (= :exact-current-issued-callback-checked-at-preparation
             (:plan-target-correspondence pre)))
      (is (= :same-node-pinned-read-matched-at-preparation
             (:external-before pre)))
      (is (= :canonical-complete-top-level-cas (:transaction-preconditions pre)))
      (is (= b (physical)) "Preparation must not submit or mutate")
      (check-nonclaims! pre)
      (is (some? ((:submit! g))))
      (is (= (after b) (physical)))
      (let [post ((:summary! g))]
        (is (= :wrapper-permit-consumed (:status post)))
        (is (= :returned-transaction-key (:underlying-gateway-status post)))
        (check-nonclaims! post))
      (reject! :one-shot-already-attempted (:submit! g))
      (is (= (after b) (physical))))))

(deftest wrong-physical-document-never-acquires-submission-authority
  (let [b (before)
        other (assoc b :xt/id "v1587-another-target")]
    (put! b)
    (reject! :command-or-protection-disagrees-with-plan-target
             #(prepare! (candidate other)))
    (is (= b (physical)))
    (is (nil? (first (xt/q *node*
                            ["SELECT * FROM requests WHERE _id = ?"
                             (:xt/id other)]))))))

(deftest wrong-command-or-protected-target-fails-before-xtdb-submission
  (let [b (before)
        command-source (candidate b)]
    (put! b)
    (reject! :command-or-protection-disagrees-with-plan-target
             #(prepare! (assoc-in command-source
                                  [:plan :commands 0 :model/operation] :cancel)))
    (reject! :command-or-protection-disagrees-with-plan-target
             #(prepare! (assoc-in command-source
                                  [:protected-documents 0 :target]
                                  [table "some-other-private-id"])))
    (is (= b (physical)))))

(deftest forged-caller-before-fails-against-pinned-external-read
  (let [b (before)
        forged (assoc b :note "invented")]
    (put! b)
    (reject! :guarded-external-prestate-acquisition-failed
             #(prepare! (candidate forged)))
    (is (= b (physical)))))

(deftest complete-top-level-guards-reject-intervening-write
  (let [b (before)]
    (put! b)
    (let [g (prepare! (candidate b))
          intervened (assoc b :note "intervened-with-same-revision")]
      (put! intervened)
      (is (= :xtdb/assert-failed (reason (:submit! g))))
      (is (= intervened (physical))
          "Complete top-level CAS must not overwrite an unrelated concurrent change")
      (is (= :wrapper-permit-consumed (:status ((:summary! g)))))
      (is (= :threw-outcome-unknown (:underlying-gateway-status ((:summary! g)))))
      (reject! :one-shot-already-attempted (:submit! g))
      (is (= intervened (physical))))))

(deftest late-target-validation-substitution-consumes-wrapper-permit
  (let [b (before)]
    (put! b)
    (let [g (prepare! (candidate b))]
      (with-redefs [target-binding/require-declared-operation-target!
                    (fn [& _] (throw (ex-info "substituted" {})))]
        (reject! :substituted-target-or-prestate-checker (:submit! g)))
      (is (= :wrapper-permit-consumed (:status ((:summary! g)))))
      (reject! :one-shot-already-attempted (:submit! g))
      (is (= b (physical)) "No physical submission follows a failed recheck"))))

(deftest unissued-or-weaker-resolver-is-not-a-plan-bound-capability
  (let [b (before)
        valid (:issuance *case*)
        fake (fn [req] ((:expected-target! valid) req))]
    (put! b)
    (reject! :unissued-or-changed-target-resolver
             #(prepare! *node* (:contract *case*)
                        {:expected-target! fake} operation request (candidate b)))
    (reject! :invalid-plan-bound-prestate-input
             #(prepare! *node* (:contract *case*)
                        (assoc valid :extra true) operation request (candidate b)))
    (is (= b (physical)))))

(deftest missing-or-incorrectly-selected-request-must-fail-closed
  (let [b (before)]
    (put! b)
    (reject! :plan-bound-target-resolution-failed
             #(prepare! *node* (:contract *case*) (:issuance *case*)
                        operation (assoc request :uri "/unknown") (candidate b)))
    (reject! :command-or-protection-disagrees-with-plan-target
             #(prepare! *node* (:contract *case*) (:issuance *case*)
                        :other/operation request (candidate b)))
    (reject! :invalid-plan-bound-prestate-input
             #(prepare! *node* (:contract *case*) (:issuance *case*)
                        :claim request (candidate b)))
    (is (= b (physical)))))

(deftest malformed-or-foreign-inputs-do-not-issue-authority
  (let [b (before)
        source (candidate b)]
    (put! b)
    (doseq [bad [nil [] {} {:expected-target! identity :unexpected true}
                 (with-meta (:issuance *case*) {:forged true})]]
      (reject! :invalid-plan-bound-prestate-input
               #(prepare! *node* (:contract *case*) bad operation request source)))
    (reject! :invalid-plan-bound-prestate-input
             #(prepare! nil (:contract *case*) (:issuance *case*)
                        operation request source))
    (reject! :invalid-plan-bound-prestate-input
             #(prepare! *node* (:contract *case*) (:issuance *case*)
                        operation (with-meta request {:forged true}) source))
    (is (= b (physical)))))

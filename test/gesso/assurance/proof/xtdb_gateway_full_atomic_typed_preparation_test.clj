(ns gesso.assurance.proof.xtdb-gateway-full-atomic-typed-preparation-test
  "v1773: typed abstract domain transition plus complete canonical domain
   and durable command ledger in one guarded, submission-disabled plan."
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.assurance.proof.xtdb-gateway-full-atomic-typed-preparation :as full]
            [gesso.assurance.proof.xtdb-gateway-typed-transition-seam :as typed]
            [gesso.model.atomic-incorporation-footprint-test]
            [gesso.model.resolved-operation-test]
            [gesso.model.command-resolution :as resolution]
            [gesso.model.tx :as tx]))

(def ^:private original-fixtures 'gesso.model.resolved-operation-test)
(def ^:private atomic-fixtures 'gesso.model.atomic-incorporation-footprint-test)
(defn- fixture [ns-name name]
  (var-get (or (ns-resolve ns-name name)
               (throw (ex-info "Missing existing fixture" {:name name})))))

(def ^:private declaration
  {:subject {:kind :operation :id :request/claim
             :interface {:operation :claim}}
   :state-types {:doc/id {:type :string}
                 :doc/revision {:type :integer}
                 :doc/created {:type :string}
                 :doc/updated {:type :string}
                 :doc/owner {:type :string}}
   :input-types {:input/owner {:type :string}}
   :guard {:readers #{:doc/owner}
           :expression [:= [:read :doc/owner] [:literal "unclaimed"]]}
   :reads #{:doc/owner :input/owner}
   :writes #{:doc/revision :doc/updated :doc/owner}
   :assignments {:doc/revision [:literal 1]
                 :doc/updated [:literal "t1"]
                 :doc/owner [:read :input/owner]}})

(def ^:private mapping
  {:doc/id :xt/id
   :doc/revision :revision
   :doc/created :created-at
   :doc/updated :updated-at
   :doc/owner :owner})

(def ^:private premises
  {:invariants
   {:doc/owner-not-blank
    {:readers #{:doc/owner}
     :expression [:not= [:read :doc/owner] [:literal ""]]}}
   :read-premises
   [{:operation-id :request/claim :invariant-id :doc/owner-not-blank
     :field :doc/owner :reader :input/owner :reader-origin :input}]})

(defn- setup []
  (let [{:keys [binding incorporated]}
        ((fixture atomic-fixtures 'case-source))]
    {:source {:binding binding
              :outcome (:gesso.command/outcome (:record incorporated))
              :now (:gesso.command/created-at (:record incorporated))
              :plan (update (:plan incorporated) :commands pop)}
     :binding binding :incorporated incorporated}))

(defn- issue
  ([source] (issue source declaration {:input/owner "helper-a"} mapping premises))
  ([source decl inputs fields proof]
   (full/issue-full-atomic-typed-preparer! source decl inputs fields proof)))

(defn- reason [thunk]
  (try (thunk) :no-error
       (catch clojure.lang.ExceptionInfo e
         (or (:reason (ex-data e)) (throw e)))))

(deftest real-original-and-independent-typed-domain-with-complete-atomic-ledger
  (let [{:keys [source incorporated]} (setup)
        writes (atom 0)
        issued (with-redefs [tx/transact!
                            (fn [& _] (swap! writes inc)
                              (throw (ex-info "no writer allowed" {})))]
                 (issue source))
        summary ((:summary! issued))
        prepared ((:prepare-once! issued))]
    (is (= full/report-type (:type issued) (:type summary) (:type prepared)))
    (is (= :issued-unsubmitted (:status summary)))
    (is (= :typed-full-atomic-prepared-not-submitted (:status prepared)))
    (is (= :request/claim (:operation-id prepared)))
    (is (= :original-and-independent-source-relative
           (:typed-domain-transition prepared)))
    (is (= :source-relative-values-screened (:original-read-premises prepared)))
    (is (= :one-domain-update-plus-durable-ledger-create
           (:atomic-commands prepared)))
    (is (= :full-plan-assert-and-write-order-checked
           (:guarded-xtdb-operations prepared)))
    (is (= :preserved-not-published (:live-inputs prepared)))
    (is (= :disabled (:physical-submission prepared)))
    (is (= :not-established (:typed-one-document-gateway-adoption prepared)))
    (is (= :not-established (:business-policy-authenticity prepared)))
    (is (= :not-established (:current-xtdb-prestate prepared)))
    (is (= :not-established (:xtdb-commit-or-receipt prepared)))
    (is (= :none (:transaction-authority prepared)))
    (is (= :not-issued (:certificate prepared)))
    (is (zero? @writes))
    (is (= 2 (count (get-in incorporated [:plan :commands]))))
    (is (not-any? #(contains? prepared %)
                  [:plan :tx-ops :source :binding :record :domain :ledger
                   :submit! :execution :request :response :candidate]))
    (is (= :preparation-permit-consumed (:status ((:summary! issued)))))
    (is (= :preparation-already-attempted (reason (:prepare-once! issued))))))

(deftest typed-declaration-change-cannot-authorize-different-domain-effect
  (let [{:keys [source]} (setup)]
    (doseq [bad [(assoc-in declaration [:guard :expression]
                           [:= [:read :doc/owner] [:literal "someone-else"]])
                 (assoc-in declaration [:assignments :doc/owner]
                           [:literal "someone-else"])
                 (assoc declaration :writes #{:doc/owner})]]
      (is (= :typed-domain-relation-not-established
             (reason #(issue source bad {:input/owner "helper-a"}
                             mapping premises)))))))

(deftest typed-declaration-cannot-hide-domain-frame-or-replace-read-premises
  (let [{:keys [source]} (setup)]
    (doseq [[decl input fields proof]
            [[declaration {:input/owner "fake"} mapping premises]
             [declaration {:input/owner 5} mapping premises]
             [declaration {:input/owner "helper-a"}
              (dissoc mapping :doc/created) premises]
             [declaration {:input/owner "helper-a"}
              (assoc mapping :doc/created :owner) premises]
             [declaration {:input/owner "helper-a"} mapping
              (assoc premises :read-premises [])]]]
      (is (= :typed-domain-relation-not-established
             (reason #(issue source decl input fields proof)))))))

(deftest two-domain-commands-are-not-misrepresented-as-one-typed-transition
  (let [{:keys [source]} (setup)
        membership ((fixture original-fixtures 'membership-document) "member-v1773")
        other ((fixture original-fixtures 'revoke-command) membership)
        bad (update-in source [:plan :commands] conj other)]
    (is (= :unsupported-domain-plus-ledger-command-shape
           (reason #(issue bad))))))

(deftest inconsistent-canonical-source-cannot-produce-a-full-plan
  (let [{:keys [source]} (setup)]
    (doseq [bad [(dissoc source :now)
                 (assoc source :extra :not-canonical)
                 (assoc-in source [:binding :principal] "untrusted")]]
      (is (#{:invalid-full-atomic-typed-source
             :invalid-canonical-incorporation}
           (reason #(issue bad)))))))

(deftest rechecked-typed-and-guarded-seams-refuse-substitution
  (let [{:keys [source]} (setup)
        issued (issue source)]
    (with-redefs [typed/check-source-relative-typed-domain!
                  (fn [& _] {:type typed/source-check-type})]
      (is (= :substituted-original-source-or-preparer
             (reason (:prepare-once! issued)))))
    (is (= :preparation-already-attempted
           (reason (:prepare-once! issued))))))

(deftest live-and-transaction-options-survive-typed-checking-without-publication
  (let [{:keys [source]} (setup)
        invoked (atom 0)
        changed (assoc-in source [:plan :entry-fn]
                          (fn [& _] (swap! invoked inc) :should-not-run))
        changed (assoc changed :plan (dissoc (:plan changed) :entry))
        issued (issue changed)
        result ((:prepare-once! issued))]
    (is (= :typed-full-atomic-prepared-not-submitted (:status result)))
    (is (= :preserved-not-published (:live-inputs result)))
    (is (zero? @invoked))
    (is (= :disabled (:physical-submission result)))))

(deftest real-xtdb-read-only-typed-preparation-before-original-atomic-submit
  ((fixture original-fixtures 'with-real-xtdb)
   (fn []
     (let [{:keys [request binding] :as d}
           ((fixture original-fixtures 'fixture-data))
           environment ((fixture original-fixtures 'real-environment) d nil)
           src {:binding binding :outcome {:status :claimed :owner "helper-a"}
                :now "t1" :plan ((fixture original-fixtures 'domain-plan) request)}]
       ((fixture original-fixtures 'seed!) [:requests request])
       (let [issued (issue src)
             report ((:prepare-once! issued))]
         (is (= :typed-full-atomic-prepared-not-submitted (:status report)))
         (is (= request
                ((fixture original-fixtures 'read-request) (:xt/id request))))
         (is (nil? ((fixture original-fixtures 'read-command)
                   (resolution/record-id binding))))
         (is (= :incorporated
                (:status ((fixture original-fixtures 'real-call) environment :submit))))
         (is (= ((fixture original-fixtures 'next-version)
                 request {:owner "helper-a"})
                ((fixture original-fixtures 'read-request) (:xt/id request))))
         (is (= (resolution/command-record
                 binding {:status :claimed :owner "helper-a"} "t1")
                ((fixture original-fixtures 'read-command)
                 (resolution/record-id binding))))
         (is (= :replay
                (:status ((fixture original-fixtures 'real-call)
                          environment :reconcile)))))))))

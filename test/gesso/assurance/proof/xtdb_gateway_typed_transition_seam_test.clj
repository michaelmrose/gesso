(ns gesso.assurance.proof.xtdb-gateway-typed-transition-seam-test
  "v1755: real-XTDB bounded integration and hostile original-source
   correspondence checks. Reuse the existing independently certified route,
   actual model update and pinned/complete-CAS fixture; do not substitute an
   in-memory fake writer. Source provenance and theorem premises remain open."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.set :as set]
            [gesso.assurance.proof.xtdb-gateway-typed-transition-seam :as seam]
            [gesso.assurance.proof.xtdb-gateway-assembled-typed-transition-seam :as assembled-seam]
            [gesso.assurance.proof.installed-route-provenance :as installed-router]
            [gesso.assurance.proof.checked-biff-host-adoption :as host]
            [gesso.live.application-preflight :as application]
            [gesso.live.application-preflight.certified-contract :as certified]
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
     ;; Retain direct low-level adversarial tests: v1757 makes this an
     ;; INTERNAL primitive, never a public source-authoritative entry point.
     ((var-get (ns-resolve 'gesso.assurance.proof.xtdb-gateway-typed-transition-seam
                           'prepare-typed-transition-one-shot!))
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

;; v1757: once a declared original source catalog is checked and captured by
;; a binder, each submission rederives its entire declared invariant inventory.
;; This is not evidence that the issuer chose the real installed app's source.
(def ^:private declared-catalog
  {:id :example/physical-typed-catalog
   :declared-operation-ids #{:example/claim}
   :initial-condition {:readers #{:doc/owner}
                       :expression [:= [:read :doc/owner] [:literal "available"]]}
   :invariants (assoc (:invariants proof-context)
                      :doc/owner-not-reserved
                      {:readers #{:doc/owner}
                       :expression [:not= [:read :doc/owner]
                                    [:literal "reserved"]]})
   :operations [{:id :example/claim
                 :construction (-> source
                                   (assoc :replacements (:assignments source)
                                          :frame (set/difference
                                                  (set (keys (:state-types source)))
                                                  (:writes source)))
                                   (dissoc :assignments))}]})

(defn- bound-preparer
  ([] (bound-preparer declared-catalog))
  ([catalog]
   (seam/issue-source-bound-typed-transition-gateway!
    catalog :example/claim)))

(defn- bound-prepare
  ([binder candidate-source]
   (bound-prepare binder candidate-source {:input/owner "claimed"}))
  ([binder candidate-source inputs]
   (let [{:keys [contract issuance]} (case-context)]
     ((:prepare! binder) (node) contract issuance
      {:request-method :post :uri "/operations/example/claim"}
      candidate-source inputs mapping))))

(deftest bounded-catalog-issues-a-real-one-shot-physical-writer
  (let [b (before)]
    (put! b)
    (let [binder (bound-preparer)
          bs ((:summary! binder))]
      (is (= seam/binding-type (:type binder) (:type bs)))
      (is (= #{:type :version :prepare! :summary!} (set (keys binder))))
      (is (= :closed-declared-source-checked (:source-catalog-binding bs)))
      (is (= :declared-only-not-production-proved (:source-completeness bs)))
      (is (= :not-established (:installed-source-provenance bs)))
      (is (= :not-accepted (:caller-selected-read-premises bs)))
      (is (not (contains? (ns-publics 'gesso.assurance.proof.xtdb-gateway-typed-transition-seam)
                          'prepare-typed-transition-one-shot!)))
      (is (= b (physical)))
      (let [prepared (bound-prepare binder (candidate b))
            summary ((:summary! prepared))]
        (is (= :closed-declared-source-checked (:source-catalog-binding summary)))
        (is (= :not-accepted (:caller-selected-read-premises summary)))
        (is (= :not-issued (:certificate summary)))
        (is (= :not-admitted (:admission summary)))
        (is (= b (physical)))
        (is (some? ((:submit! prepared))))
        (is (= (after b) (physical)))
        (is (= :source-bound-one-shot-already-attempted
               (reason (:submit! prepared))))))))

(deftest the-source-catalog-not-a-per-call-premise-list-controls-physical-read
  (let [b (before)]
    (put! b)
    (let [binder (bound-preparer)
          ; a well-typed string, allowed by the original v1756
          ; supplied context's :not-empty but forbidden by catalog :not-reserved
          src (candidate b (assoc (after b) :owner "reserved"))]
      (is (= :source-read-premise-failed
             (reason #(bound-prepare binder src {:input/owner "reserved"}))))
      (is (= b (physical)))
      ;; The public gateway accepts only seven preparation inputs; neither
      ;; caller-selected proof-context nor a new source declaration is allowed.
      (is (= clojure.lang.ArityException
             (try (let [{:keys [contract issuance]} (case-context)]
                    ((:prepare! binder) (node) contract issuance
                     {:request-method :post :uri "/operations/example/claim"}
                     src {:input/owner "reserved"} mapping
                     {:read-premises []}))
                  nil
                  (catch clojure.lang.ArityException e (class e)))))
      (is (= :source-bound-one-shot-already-attempted
             (let [prepared (bound-prepare binder (candidate b))]
               ((:submit! prepared))
               (reason (:submit! prepared))))))))

(deftest declared-catalog-must-rederive-every-original-obligation
  (let [b (before)]
    (put! b)
    (doseq [bad [(assoc declared-catalog :declared-operation-ids
                        #{:example/unclaim})
                 (assoc-in declared-catalog [:operations 0 :id]
                           :example/unclaim)
                 (update declared-catalog :operations conj
                         (first (:operations declared-catalog)))
                 (assoc-in declared-catalog [:operations 0 :construction :frame]
                           #{:doc/note})
                 (assoc-in declared-catalog [:invariants :doc/owner-not-empty :readers]
                           #{:doc/nonexistent})]]
      (is (= :invalid-closed-declared-source-catalog
             (reason #(bound-preparer bad)))))
    (is (= b (physical)))))

(deftest bound-gateway-rechecks-catalog-interpreter-on-one-permit
  (let [b (before)]
    (put! b)
    (let [binder (bound-preparer)
          g (bound-prepare binder (candidate b))]
      (with-redefs [gesso.assurance.proof.operation-set/check-operation-set!
                    (fn [& _] {:type :forged})]
        (is (= :substituted-source-or-physical-interpreter
               (reason (:submit! g)))))
      (is (= b (physical)))
      (is (= :source-bound-one-shot-already-attempted (reason (:submit! g)))))))

(deftest closed-declared-catalog-remains-an-unproved-issuer-intent
  (let [b (before)]
    (put! b)
    (let [weaker (update declared-catalog :invariants
                         dissoc :doc/owner-not-reserved)
          weaker-binder (bound-preparer weaker)]
      ;; Reproves closure RELATIVE TO THIS newly selected source, not that the
      ;; source is the intended application or includes all required rules.
      (is (= :not-established (:installed-source-provenance
                               ((:summary! weaker-binder)))))
      (is (= :declared-only-not-production-proved
             (:source-completeness ((:summary! weaker-binder)))))
      (is (= :source-relative-values-screened-not-proof-established
             (:source-read-theorem-premises
              ((:summary! (bound-prepare weaker-binder
                                         (candidate b (assoc (after b) :owner "reserved"))
                                         {:input/owner "reserved"}))))))
      (is (= b (physical))))))

;; v1758: the caller's declared source must match the CURRENT independently
;; assembled operation/issued handler/registered in-process route universe.
;; This is not evidence that Biff/HTTP actually adopted the checked router.
(defn- assembled-preparer
  ([] (assembled-preparer declared-catalog))
  ([catalog]
   (let [{:keys [contract]} (case-context)
         router (installed-router/install-checked-ring-router! contract)]
     (assembled-seam/issue-assembled-source-bound-typed-transition-gateway!
      catalog contract router :example/claim))))

(defn- assembled-prepare
  ([binder candidate-source]
   (assembled-prepare binder candidate-source
                     {:request-method :post :uri "/operations/example/claim"}))
  ([binder candidate-source request]
   (let [{:keys [issuance]} (case-context)]
     ((:prepare! binder) (node) issuance request candidate-source
      {:input/owner "claimed"} mapping))))

(deftest assembled-source-preparer-reuses-real-protected-xtdb-path
  (let [b (before)]
    (put! b)
    (let [binder (assembled-preparer)
          source-summary ((:summary! binder))
          prepared (assembled-prepare binder (candidate b))
          summary ((:summary! prepared))]
      (is (= assembled-seam/binding-type (:type binder) (:type source-summary)))
      (is (= :closed-declared-set-equals-assembly
             (:source-operation-coverage source-summary)))
      (is (= :exact-current-assembly-certified-handlers-and-local-router
             (:assembled-source-binding summary)))
      (is (= :not-established (:installed-router-adoption summary)))
      (is (= :not-established (:original-business-policy-authenticity summary)))
      (is (= :not-established (:complete-deployed-writer-coverage summary)))
      (is (= :not-issued (:production-certification summary)))
      (is (= b (physical)))
      (is (some? ((:submit! prepared))))
      (is (= (after b) (physical)))
      (is (= :assembled-one-shot-already-attempted
             (reason (:submit! prepared)))))))

(deftest assembled-seam-refuses-caller-selected-catalog-op-divergence
  (let [b (before)]
    (put! b)
    (let [other (assoc-in (first (:operations declared-catalog))
                          [:construction :subject :id] :example/other)
          extra (-> declared-catalog
                    (update :declared-operation-ids conj :example/other)
                    (update :operations conj
                            {:id :example/other
                             :construction other}))]
      (is (= :invalid-original-operation-catalog
             (reason #(assembled-preparer extra))))
      (is (= :declared-catalog-assembly-or-router-closure-not-established
             (reason #(let [{:keys [contract]} (case-context)
                            router (installed-router/install-checked-ring-router! contract)]
                        (assembled-seam/issue-assembled-source-bound-typed-transition-gateway!
                         declared-catalog contract router :example/unclaim)))))
      (is (= b (physical))))))

(deftest assembled-seam-rejects-incorrect-route-before-physical-preparation
  (let [b (before)]
    (put! b)
    (let [binder (assembled-preparer)]
      (doseq [request [{:request-method :post :uri "/wrong"}
                       {:request-method :get :uri "/operations/example/claim"}
                       {:request-method :post :uri "/operations/example/claim/other"}]]
        (is (= :request-is-not-a-bound-operation-route
               (reason #(assembled-prepare binder (candidate b) request)))))
      (is (= b (physical))))))

(deftest assembled-seam-refuses-unregistered-or-stale-in-process-routers
  (let [b (before)
        {:keys [contract issuance]} (case-context)]
    (put! b)
    (is (= :unregistered-or-stale-ring-router
           (reason #(assembled-seam/issue-assembled-source-bound-typed-transition-gateway!
                     declared-catalog contract (fn [_] {:status 200}) :example/claim))))
    (let [router (installed-router/install-checked-ring-router! contract)
          binder (assembled-seam/issue-assembled-source-bound-typed-transition-gateway!
                  declared-catalog contract router :example/claim)]
      ;; A swapped checker can neither launder a source report nor get past the
      ;; one-use submission recheck. The underlying XTDB writer is untouched.
      (with-redefs [gesso.live.application-preflight.certified-contract/check-certified-contracts!
                    (fn [_] {:type :forged})]
        (is (= :substituted-application-or-source-checker
               (reason #((:summary! binder))))))
      (let [prepared ((:prepare! binder) (node) issuance
                      {:request-method :post :uri "/operations/example/claim"}
                      (candidate b) {:input/owner "claimed"} mapping)]
        (with-redefs [gesso.live.application-preflight.certified-contract/check-certified-contracts!
                      (fn [_] {:type :forged})]
          (is (= :substituted-application-or-source-checker
                 (reason (:submit! prepared)))))
        (is (= :assembled-one-shot-already-attempted
               (reason (:submit! prepared)))))
      (is (= b (physical))))))

(deftest assembled-route-check-does-not-authenticate-weaker-original-policy
  (let [b (before)
        weaker (update declared-catalog :invariants dissoc :doc/owner-not-reserved)]
    (put! b)
    (let [binder (assembled-preparer weaker)
          summary ((:summary! binder))]
      ;; An existing route/plan checks the *same operation ID*, not whether the
      ;; supplied catalog contains all of the business's intended rules.
      (is (= :not-established (:original-business-policy-authenticity summary)))
      (is (= :not-established (:installed-router-adoption summary)))
      (is (= :closed-declared-set-equals-assembly
             (:source-operation-coverage summary)))
      (is (= b (physical))))))


;; v1759: Reuse the actual Biff module lifecycle and its privately recorded
;; checked router rather than allowing a caller to nominate another router.
(def ^:dynamic *host-typed-biff-modules* [])

(defn- host-typed-preparer
  ([] (host-typed-preparer declared-catalog))
  ([catalog]
   (let [{:keys [contract]} (case-context)
         issued (host/issue-checked-biff-host-module! contract)
         modules [application/application-handler-module (:module issued)]
         system (binding [*host-typed-biff-modules* modules]
                  (application/start-biff-application!
                   (:application-assembly contract) {}
                   #'*host-typed-biff-modules*
                   [application/application-handler-module-id host/module-id]))
         builder (host/issue-host-adopted-assembled-typed-transition-gateway!
                  (:handle issued) contract system catalog :example/claim)]
     {:builder builder :issued issued :system system})))

(deftest adopted-host-typed-gateway-reuses-checked-real-xtdb-submission
  (let [b (before)]
    (put! b)
    (let [{:keys [builder issued system]} (host-typed-preparer)
          status ((:summary! builder))
          prepared (assembled-prepare builder (candidate b))
          summary ((:summary! prepared))]
      (is (= host/host-typed-binding-type (:type builder) (:type status)))
      (is (= :exact-issued-checked-router-under-canonical-wrapper
             (:host-slot-binding status) (:host-slot-binding summary)))
      (is (= :observed-once (:host-module-start status)))
      (is (= :complete-current-certified-literal-routes
             (:route-coverage status)))
      (is (= :not-established (:deployed-http-handler-adoption status)))
      (is (= :not-established (:deployed-http-handler-adoption summary)))
      (is (= :not-established (:original-business-policy-authenticity summary)))
      (is (= :not-established (:complete-deployed-writer-coverage summary)))
      (is (= :not-issued (:production-certification summary)))
      (is (fn? (:biff.ring/handler system)))
      (is (= host/report-type
             (:type (host/require-current-biff-host-adoption!
                     (:handle issued) (:contract (case-context)) system))))
      (is (= b (physical)) "Neither Biff startup nor preparation writes")
      (is (some? ((:submit! prepared))))
      (is (= (after b) (physical)))
      (is (= :host-typed-one-shot-already-attempted
             (reason (:submit! prepared)))))))

(deftest host-typed-gateway-rejects-altered-slot-and-unissued-handles
  (let [b (before)
        {:keys [contract]} (case-context)]
    (put! b)
    (let [{:keys [issued system]} (host-typed-preparer)
          attempt (fn [handle system-value catalog]
                    (host/issue-host-adopted-assembled-typed-transition-gateway!
                     handle contract system-value catalog :example/claim))]
      (is (= :host-handler-slot-mismatch
             (reason #(attempt (:handle issued)
                               (assoc system :biff.ring/handler (fn [_] {:status 200}))
                               declared-catalog))))
      (is (= :unissued-host-adoption
             (reason #(attempt (host/->HostAdoptionHandle)
                               system declared-catalog))))
      (is (= :changed-host-adoption-source
             (reason #(host/issue-host-adopted-assembled-typed-transition-gateway!
                       (:handle issued) (into {} contract) system
                       declared-catalog :example/claim))))
      (is (= b (physical))))))

(deftest host-typed-gateway-rechecks-before-submission-and-consumes-permit
  (let [b (before)]
    (put! b)
    (let [{:keys [builder]} (host-typed-preparer)
          prepared (assembled-prepare builder (candidate b))]
      (with-redefs [gesso.assurance.proof.xtdb-gateway-assembled-typed-transition-seam/issue-assembled-source-bound-typed-transition-gateway!
                    (fn [& _] :forged)]
        (is (= :substituted-host-or-assembled-boundary
               (reason (:submit! prepared)))))
      (is (= :host-typed-one-shot-already-attempted
             (reason (:submit! prepared))))
      (is (= b (physical))))))

(deftest host-typed-route-check-is-not-original-policy-authentication
  (let [b (before)
        weaker (update declared-catalog :invariants dissoc :doc/owner-not-reserved)]
    (put! b)
    (let [{:keys [builder]} (host-typed-preparer weaker)
          summary ((:summary! builder))]
      (is (= :not-established (:original-business-policy-authenticity summary)))
      (is (= :not-established (:deployed-http-handler-adoption summary)))
      (is (= b (physical))))))


;; v1760: execute the CURRENT Biff handler slot, not a synthetic Ring result.
;; This gives process-local invocation evidence only. It is deliberately NOT
;; proof that this HTTP request caused the independent guarded XTDB submit!.
(deftest host-typed-observation-invokes-the-current-guarded-biff-handler
  (let [b (before)]
    (put! b)
    (let [{:keys [builder system]} (host-typed-preparer)
          observe! (:observe-handler! builder)
          request {:request-method :post :uri "/operations/example/claim"
                   :request-id :v1760-observed-handler}
          before-report ((:summary! builder))
          report (observe! request)]
      (is (= :available-one-shot-not-yet-attempted
             (:handler-dispatch before-report)))
      (is (= :one-shot-attempt-consumed
             (:handler-dispatch ((:summary! builder)))))
      (is (fn? (:biff.ring/handler system)))
      (is (= host/host-typed-binding-type (:type report)))
      (is (= :example/claim (:operation-id report)))
      (is (= :one-in-process-biff-handler-slot-invocation (:dispatch report)))
      (is (= :one-exact-issued-handler-executor-returned
             (:issued-executor-invocation report)))
      (is (= :one-exact-issued-handler-executor-returned
             (:issued-executor-invocation ((:summary! builder)))))
      (is (= 200 (:ring-response-status report)))
      (is (= :not-established (:request-principal-authenticity report)))
      (is (= :not-established (:handler-dispatch-to-xtdb-causality report)))
      (is (= :not-established (:deployed-http-handler-adoption report)))
      (is (= :not-established (:original-business-policy-authenticity report)))
      (is (= :not-established (:complete-deployed-writer-coverage report)))
      (is (= :not-issued (:certificate report)))
      (is (not-any? #(contains? report %)
                    [:request :response :handler :system :contract :router
                     :witness :node :executor :transaction]))
      (is (= b (physical)) "Biff invocation does not certify or submit this XTDB write")
      (is (= :host-handler-observation-already-attempted
             (reason #(observe! request)))))))

(deftest host-typed-observation-fails-closed-on-wrong-route
  (let [b (before)]
    (put! b)
    (doseq [bad-request [{:request-method :get :uri "/operations/example/claim"}
                         {:request-method :post :uri "/wrong"}
                         {:request-method :post :uri "/operations/example/claim/more"}
                         {:request-method :post :uri nil}
                         {}]]
      (let [{:keys [builder]} (host-typed-preparer)
            observe! (:observe-handler! builder)]
        (is (= :request-is-not-a-bound-operation-route
               (reason #(observe! bad-request))))
        (is (= :host-handler-observation-already-attempted
               (reason #(observe! {:request-method :post
                                   :uri "/operations/example/claim"}))))))
    (is (= b (physical)))))

(deftest host-typed-observation-consumes-permit-on-substituted-source
  (let [b (before)]
    (put! b)
    (let [{:keys [builder]} (host-typed-preparer)
          observe! (:observe-handler! builder)
          request {:request-method :post :uri "/operations/example/claim"}]
      (with-redefs [gesso.live.application-preflight/operation-summary
                    (fn [& _] {:example/claim {:routes []}})]
        (is (= :substituted-application-or-route-checker
               (reason #(observe! request)))))
      (is (= :host-handler-observation-already-attempted
             (reason #(observe! request))))
      (is (= b (physical))))))

;; v1761: A successful Ring status alone cannot impersonate the issued
;; certified executor's actual call. The callback is counted only after its
;; real executor returns with the same exact request object.
(deftest issued-executor-observer-refuses-synthetic-success
  (let [b (before)
        {:keys [contract]} (case-context)
        handler (get-in contract [:semantic-route-handlers :example/claim])
        request {:request-method :post :uri "/operations/example/claim"}]
    (put! b)
    (is (= :missing-or-ambiguous-issued-executor-invocation
           (reason #(certified/observe-issued-executor-invocation!
                     handler request (fn [] {:status 200})))))
    (is (= :missing-or-ambiguous-issued-executor-invocation
           (reason #(certified/observe-issued-executor-invocation!
                     handler request
                     (fn [] (handler (assoc request :different true)))))))
    (is (= :missing-or-ambiguous-issued-executor-invocation
           (reason #(certified/observe-issued-executor-invocation!
                     handler request
                     (fn [] (handler request)
                       (handler request) {:status 200})))))
    (is (= b (physical)))))

(deftest issued-executor-observer-refuses-nested-claims
  (let [b (before)
        {:keys [contract]} (case-context)
        handler (get-in contract [:semantic-route-handlers :example/claim])
        request {:request-method :post :uri "/operations/example/claim"}]
    (put! b)
    (is (= :nested-issued-executor-observation
           (reason #(certified/observe-issued-executor-invocation!
                     handler request
                     (fn []
                       (certified/observe-issued-executor-invocation!
                        handler request (fn [] (handler request))))))))
    (is (= b (physical)))))

(deftest host-typed-observation-proves-executor-dispatch-not-xtdb-causality
  (let [b (before)]
    (put! b)
    (let [{:keys [builder]} (host-typed-preparer)
          report ((:observe-handler! builder)
                  {:request-method :post :uri "/operations/example/claim"})]
      (is (= :one-exact-issued-handler-executor-returned
             (:issued-executor-invocation report)))
      (is (= :not-established (:handler-dispatch-to-xtdb-causality report)))
      (is (= :not-established (:deployed-http-handler-adoption report)))
      (is (= :not-established (:request-principal-authenticity report)))
      (is (= b (physical))
          "A certified executor invocation is not an XTDB commit"))))


;; v1765: The source-bound gateway itself emits this opt-in trace. An
;; unrelated adapter report or caller-supplied success value is insufficient.
(deftest source-bound-gateway-return-is-captured-with-real-xtdb
  (let [b (before)]
    (put! b)
    (let [g (bound-prepare (bound-preparer) (candidate b))
          {:keys [execution report]}
          (seam/observe-source-bound-typed-submission!
           :example/claim (fn [] ((:submit! g))))]
      (is (= :returned (:outcome execution)))
      (is (some? (:result execution)))
      (is (= seam/source-bound-trace-type (:type report)))
      (is (= seam/source-bound-trace-version (:version report)))
      (is (= :one-source-bound-typed-submit-return-observed (:status report)))
      (is (= :observed (:source-bound-gateway-return report)))
      (is (= :not-established (:installed-biff-handler-invocation report)))
      (is (= :not-established (:gateway-to-adapter-causality report)))
      (is (= :not-established (:xtdb-transaction-committed report)))
      (is (= :not-established (:original-business-policy-authenticity report)))
      (is (= :not-issued (:certificate report)))
      (is (= :none (:authority report)))
      (is (not-any? #(contains? report %)
                    [:execution :result :error :token :events :source :node
                     :request :response :transaction :prepared :submit!]))
      (is (= (after b) (physical)))
      (is (= :source-bound-one-shot-already-attempted
             (reason (:submit! g)))))))

(deftest synthetic-success-and-unbound-physical-gateway-do-not-qualify
  (let [b (before)]
    (put! b)
    (let [{:keys [report execution]}
          (seam/observe-source-bound-typed-submission!
           :example/claim (fn [] {:status 200 :committed true}))]
      (is (= :returned (:outcome execution)))
      (is (= :no-source-bound-typed-submit-observed (:status report)))
      (is (= :not-established (:source-bound-gateway-return report)))
      (is (= b (physical))))
    ;; Even the *real* low-level XTDB writer cannot impersonate a v1757
    ;; source-bound gateway, though it updates physical state on success.
    (let [g (prepare (candidate b))
          {:keys [report]}
          (seam/observe-source-bound-typed-submission!
           :example/claim (fn [] ((:submit! g))))]
      (is (= :no-source-bound-typed-submit-observed (:status report)))
      (is (= :not-established (:source-bound-gateway-return report)))
      (is (= (after b) (physical))))))

(deftest wrong-operation-is-not-a-source-bound-gateway-witness
  (let [b (before)]
    (put! b)
    (let [g (bound-prepare (bound-preparer) (candidate b))
          {:keys [report]}
          (seam/observe-source-bound-typed-submission!
           :example/cancel (fn [] ((:submit! g))))]
      (is (= :ambiguous-or-mismatched-source-bound-submission
             (:status report)))
      (is (= :not-established (:source-bound-gateway-return report)))
      (is (= (after b) (physical))))))

(deftest uncertain-cas-failure-is-never-promoted-to-gateway-return
  (let [b (before)]
    (put! b)
    (let [g (bound-prepare (bound-preparer) (candidate b))
          intervened (assoc b :note "v1765-cas-intervened")]
      (put! intervened)
      (let [{:keys [report execution]}
            (seam/observe-source-bound-typed-submission!
             :example/claim (fn [] ((:submit! g))))]
        (is (= :threw (:outcome execution)))
        (is (= :xtdb/assert-failed
               (or (:xtdb.error/code (ex-data (:error execution)))
                   (:reason (ex-data (:error execution))))))
        (is (= :submission-or-invocation-threw (:status report)))
        (is (= :not-established (:source-bound-gateway-return report)))
        (is (= intervened (physical)))
        (is (= :source-bound-one-shot-already-attempted
               (reason (:submit! g))))))))

(deftest nested-source-bound-submission-capture-is-rejected
  (let [b (before)]
    (put! b)
    (let [{:keys [report]}
          (seam/observe-source-bound-typed-submission!
           :example/claim
           (fn []
             (is (= :nested-source-bound-submission-observer
                    (reason #(seam/observe-source-bound-typed-submission!
                              :example/claim (fn [] :forged)))))
             :ok))]
      (is (= :no-source-bound-typed-submit-observed (:status report)))
      (is (= b (physical))))))

(deftest two-typed-gateway-attempts-cannot-masquerade-as-one-return
  (let [b (before)]
    (put! b)
    (let [binder (bound-preparer)
          first-g (bound-prepare binder (candidate b))
          stale-g (bound-prepare binder (candidate b))
          {:keys [report execution]}
          (seam/observe-source-bound-typed-submission!
           :example/claim
           (fn []
             ((:submit! first-g))
             (is (= :xtdb/assert-failed (reason (:submit! stale-g))))
             :completed))]
      (is (= :returned (:outcome execution)))
      (is (= :ambiguous-or-mismatched-source-bound-submission
             (:status report)))
      (is (= :not-established (:source-bound-gateway-return report)))
      (is (= (after b) (physical)))
      (is (= :source-bound-one-shot-already-attempted
             (reason (:submit! first-g))))
      (is (= :source-bound-one-shot-already-attempted
             (reason (:submit! stale-g)))))))

(deftest cross-thread-typed-submit-does-not-qualify-for-synchronous-capture
  (let [b (before)]
    (put! b)
    (let [g (bound-prepare (bound-preparer) (candidate b))
          {:keys [report execution]}
          (seam/observe-source-bound-typed-submission!
           :example/claim
           (fn [] @(future ((:submit! g)))))]
      (is (= :returned (:outcome execution)))
      (is (= :ambiguous-or-mismatched-source-bound-submission
             (:status report)))
      (is (= :not-established (:source-bound-gateway-return report)))
      (is (= (after b) (physical))))))

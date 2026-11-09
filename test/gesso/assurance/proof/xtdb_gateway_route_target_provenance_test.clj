(ns gesso.assurance.proof.xtdb-gateway-route-target-provenance-test
  "v1579/v1583: permanent adversarial regression for v1578's
   source-backed, literal-route, fixed-document XTDB target resolver,
   including v1582's certified-plan command-operation join.

   Uses the existing genuine certified two-operation Biff application fixture
   rather than synthetic certificate reports. Checks closed route coverage,
   exact request selection, physical-target vocabulary, issuance identity,
   helper substitution, redaction, and compatibility with v1574's actual
   command-to-document structural comparison.

   This does NOT establish startup declaration authenticity, dynamic document
   binding, user authorization, deployment adoption, physical transaction
   causality, or full business-rule refinement. v1580's source-backed
   mediator requires an issued resolver; legacy and callback-based
   mediation retain narrower guarantees."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [gesso.assurance.proof.xtdb-gateway-biff-parallel-multiroute-test :as multi]
            [gesso.assurance.proof.xtdb-gateway-operation-target-binding :as binding]
            [gesso.assurance.proof.xtdb-gateway-route-target-provenance :as provenance]
            [gesso.live.application-preflight :as application]
            [gesso.live.application-preflight.certified-contract :as certified]
            [gesso.model.command :as command])
  (:import (java.math BigInteger)
           (java.util UUID)))

(def ^:private private-claim-id "private-v1579-claim-identity")
(def ^:private private-cancel-id "private-v1579-cancel-identity")
(def ^:private private-canary "private-v1579-should-not-appear-in-diagnostics")

(def ^:private declarations
  {multi/claim-route {:command-operation :claim
                      :target [:claims private-claim-id]}
   multi/cancel-route {:command-operation :claim
                       :target [:claims private-cancel-id]}})

(def ^:private claim-request
  {:request-method :post :uri "/operations/example/claim"})
(def ^:private cancel-request
  {:request-method :post :uri "/operations/example/cancel"})

(defn- checked-source []
  (:source multi/*case*))

(defn- issue!
  ([] (issue! declarations))
  ([decls]
   (provenance/issue-certified-literal-route-target-resolver!
    (checked-source) decls)))

(defn- issued! []
  (:expected-target! (issue!)))

(defn- captured [f]
  (try
    {:accepted (f)}
    (catch clojure.lang.ExceptionInfo e
      {:message (ex-message e) :data (ex-data e)})))

(defn- assert-rejection! [reason f]
  (let [{:keys [accepted data message] :as observation} (captured f)]
    (is (not (contains? observation :accepted)) "Must reject rather than return a resolver or target")
    (is (= true (:xtdb-gateway-route-target-provenance/rejection data)))
    (is (= reason (:reason data)))
    (is (= #{:xtdb-gateway-route-target-provenance/rejection :reason}
           (set (keys data)))
        "Reject without including private targets, contract source, or requests")
    (is (= "Invalid source-linked XTDB route target provenance." message))
    (is (not (str/includes? (str message " " (pr-str data)) private-canary)))
    (is (not (str/includes? (str message " " (pr-str data)) private-claim-id)))
    (is (not (str/includes? (str message " " (pr-str data)) private-cancel-id)))))

(defn- source-for [id]
  (let [before {:xt/id id :revision 0 :created-at "t0"
                :updated-at "t0" :owner :none}
        after (assoc before :revision 1 :updated-at "t1" :owner :new)]
    {:plan {:commands [(command/update-command
                        :claims :claim before after
                        {:revision-key :revision
                         :created-at-key :created-at
                         :updated-at-key :updated-at})]
            :emit false}
     :protected-documents [{:target [:claims id] :fields :all}]}))

(defn- existing-certified-multiroute-fixture [run]
  (let [fixture (first (:clojure.test/once-fixtures
                        (meta (the-ns
                               'gesso.assurance.proof.xtdb-gateway-biff-parallel-multiroute-test))))]
    (when-not (fn? fixture)
      (throw (ex-info "Missing actual certified Biff fixture"
                      {:reason :missing-certified-multiroute-fixture})))
    (fixture run)))

(use-fixtures :once existing-certified-multiroute-fixture)

(deftest issued-source-reports-only-conditional-assurance
  (let [source (checked-source)
        resolver (issued!)
        report (provenance/require-current-certified-literal-route-target-resolver!
                resolver source)
        printed (pr-str report)]
    (is (= provenance/report-type (:type report)))
    (is (= provenance/report-version (:version report)))
    (is (= :conditional-certified-literal-route-static-target-resolver
           (:status report)))
    (is (= 2 (:certified-route-count report)))
    (is (true? (:every-declared-route-certified? report)))
    (is (= :rederived-from-current-certified-contract
           (:operation-identity report)))
    (is (= :joined-to-current-certified-plan-source
           (:command-operation-identity report)))
    (is (= :closed-startup-route-declaration (:target-identity report)))
    (is (false? (:candidate-command-used-to-derive-target? report)))
    (is (false? (:request-document-parameters-used? report)))
    (is (= :exact-ring-method-and-literal-uri (:request-match report)))
    (is (= :application-premise-not-proved (:static-declaration-authenticity report)))
    (is (= :not-established (:physical-target-authorization report)))
    (is (= :not-established (:target-bound-biff-mediator-adoption report)))
    (is (= :not-established (:abstract-operation-to-command-refinement report)))
    (is (= :not-established (:physical-transaction-causality report)))
    (is (= #{:startup-target-declarations-are-independently-correct-and-authorized
             :ring-method-and-uri-provenance
             :no-dynamic-document-target-binding-supported
             :target-bound-biff-mediator-must-require-issued-resolver
             :transaction-commit-causality-and-physical-refinement
             :external-writer-coverage-not-established}
           (:open-obligations report)))
    (is (= :not-issued (:certificate report)))
    (is (= :not-admitted (:admission report)))
    (is (= :none (:transaction-authority report) (:authority report)))
    (is (not-any? #(contains? report %)
                  [:target :source :resolver :route-target-declarations
                   :declarations :contract-source :expected-target!]))
    (doseq [secret [private-claim-id private-cancel-id private-canary]]
      (is (not (str/includes? printed secret))))))

(deftest exact-literal-route-selects-certified-operation-and-closed-target
  (let [resolver (issued!)
        claim (resolver claim-request)
        cancel (resolver cancel-request)]
    (is (= {:operation-id multi/claim
            :command-operation :claim
            :target [:claims private-claim-id]} claim))
    (is (= {:operation-id multi/cancel
            :command-operation :claim
            :target [:claims private-cancel-id]} cancel))
    (is (not= (:operation-id claim) (:operation-id cancel)))
    (is (not= (:target claim) (:target cancel)))
    (is (= claim
           (resolver (assoc claim-request :params {:id private-cancel-id
                                                  :target [:claims private-cancel-id]}))))
    (is (= claim
           (resolver (assoc claim-request :query-params {:id private-cancel-id}
                                                   :candidate-source (source-for private-cancel-id)))))
    (is (= cancel
           (resolver (assoc cancel-request :path-params {:id private-claim-id}))))))

(deftest original-command-comparison-rejects-physical-target-substitution
  (let [resolver (issued!)
        claim (resolver claim-request)
        cancel (resolver cancel-request)
        good (source-for private-claim-id)
        wrong (source-for private-cancel-id)
        report (binding/require-declared-operation-target! multi/claim claim good)]
    (is (= :conditional-declared-command-target-match (:status report)))
    (is (= :caller-supplied-not-authenticated (:expected-target-provenance report))
        "The structural validator does not upgrade the issued resolver to certified provenance")
    (is (= :not-established (:abstract-operation-to-command-semantics report)))
    (is (= :not-issued (:certificate report)))
    (is (= :none (:transaction-authority report)))
    (is (= :command-target-disagrees-with-expectation
           (get-in (captured #(binding/require-declared-operation-target!
                              multi/claim claim wrong)) [:data :reason])))
    (is (= :selected-operation-disagrees-with-expectation
           (get-in (captured #(binding/require-declared-operation-target!
                              multi/claim cancel wrong)) [:data :reason])))))

(deftest unmatched-routes-fail-closed-without-falling-back
  (let [resolver (issued!)
        invalid [nil [] {:uri "/operations/example/claim"}
                 {:request-method :post}
                 {:request-method :get :uri "/operations/example/claim"}
                 {:request-method :put :uri "/operations/example/cancel"}
                 {:request-method :post :uri "/operations/example/claim/"}
                 {:request-method :post :uri "/operations/example/CLAIM"}
                 {:request-method :post :uri "/operations/example/missing"}
                 {:request-method "post" :uri "/operations/example/claim"}
                 {:request-method :post :uri "/operations/example/claim?x=1"}
                 {:request-method :post :uri private-canary}
                 (with-meta claim-request {:injected :yes})]]
    (doseq [request invalid]
      (testing (str "invalid request shape/selector: " (pr-str request))
        (assert-rejection! :unmatched-certified-literal-request
                           #(resolver request))))
    (is (= [:claims private-claim-id] (:target (resolver claim-request))))))

(deftest incomplete-or-foreign-route-declarations-are-rejected
  (let [invalid [nil [] {} (dissoc declarations multi/claim-route)
                 (dissoc declarations multi/cancel-route)
                 (assoc declarations :route/foreign
                        {:command-operation :claim :target [:claims "other"]})
                 (assoc declarations :unqualified
                        {:command-operation :claim :target [:claims "other"]})
                 (assoc-in declarations [multi/claim-route :command-operation] nil)
                 (assoc-in declarations [multi/claim-route :command-operation] "claim")
                 (update declarations multi/claim-route dissoc :command-operation)
                 (assoc-in declarations [multi/claim-route :unexpected] true)
                 (assoc-in declarations [multi/claim-route :target] nil)
                 (assoc declarations multi/claim-route :not-an-entry)
                 (with-meta declarations {:spoofed true})
                 (assoc declarations multi/claim-route
                        (with-meta (get declarations multi/claim-route)
                          {:spoofed true}))
                 (into (sorted-map) declarations)]]
    (doseq [candidate invalid]
      (assert-rejection! :invalid-or-incomplete-static-route-target-declarations
                         #(issue! candidate)))))

(deftest only-narrow-canonical-physical-target-vocabulary-is-accepted
  (let [bad-targets [nil [] [:claims] [:claims "x" :extra]
                     [:claims nil] [:claims ""] [:claims (apply str (repeat 513 "x"))]
                     [:claims :not-an-id] [:claims {:id private-canary}]
                     [:claims [private-canary]] [:claims #{"x"}]
                     [:claims (bigint 4)] [:claims (BigInteger. "4")]
                     [:claims 1.5] [:claims true]
                     [:qualified/claims "x"] [:bad-table-name "x"]
                     [(keyword "claims$bad") "x"] ["claims" "x"]
                     (list :claims "x")
                     (with-meta [:claims "x"] {:spoofed true})]]
    (doseq [target bad-targets]
      (assert-rejection! :invalid-or-incomplete-static-route-target-declarations
                         #(issue! (assoc-in declarations [multi/claim-route :target]
                                            target))))
    ;; The documented vocabulary permits both UUID and ordinary integer IDs.
    (doseq [target [[:claims (UUID/randomUUID)] [:claims 23] [:claims -1]
                    [:my_table "a"]]]
      (let [r (:expected-target!
               (issue! (assoc-in declarations [multi/claim-route :target] target)))]
        (is (= target (:target (r claim-request))))))))

(deftest certified-contract-shape-and-coverage-cannot-be-supplied-by-assertion
  (let [contract (checked-source)]
    (doseq [bad [(assoc contract :uncertified-extra true)
                 (dissoc contract :application-assembly)
                 (dissoc contract :semantic-route-handlers)
                 (dissoc contract :certified-operations)
                 (with-meta contract {:spoofed true})
                 nil]]
      (assert-rejection! :invalid-certified-route-source
                         #(provenance/issue-certified-literal-route-target-resolver!
                           bad declarations)))
    (assert-rejection! :requires-complete-unique-certified-literal-routes
                       #(provenance/issue-certified-literal-route-target-resolver!
                         (update contract :certified-operations dissoc multi/cancel)
                         declarations))))

(deftest provenance-requires-an-exact-issued-resolver-and-original-source
  (let [contract (checked-source)
        resolver (issued!)
        before (resolver claim-request)
        changed (assoc contract :untrusted-private-field private-canary)]
    (doseq [unissued [nil identity (fn [_] before) (constantly before) {}]]
      (assert-rejection! :unissued-target-resolver
                         #(provenance/require-current-certified-literal-route-target-resolver!
                           unissued contract)))
    (assert-rejection! :changed-target-resolver-source
                       #(provenance/require-current-certified-literal-route-target-resolver!
                         resolver changed))
    (is (= private-claim-id (second (:target (resolver claim-request)))))
    (is (= 2 (:certified-route-count
              (provenance/require-current-certified-literal-route-target-resolver!
               resolver contract))))
    ;; Persistent updates to the caller's declaration map do not mutate the
    ;; closed table captured by a previously issued resolver.
    (let [replacement (assoc-in declarations [multi/claim-route :target]
                                [:claims private-cancel-id])]
      (is (= private-cancel-id
             (second (:target ((:expected-target! (issue! replacement))
                               claim-request)))))
      (is (= private-claim-id
             (second (:target (resolver claim-request))))))))

(deftest substitution-of-certification-or-route-summary-helper-fails-closed
  (let [source (checked-source)
        resolver (issued!)]
    (with-redefs [certified/check-certified-contracts! (fn [& _] {:route-ids #{}})]
      (assert-rejection! :substituted-certification-or-route-checker
                         #(issue!))
      (assert-rejection! :substituted-certification-or-route-checker
                         #(resolver claim-request))
      (assert-rejection! :substituted-certification-or-route-checker
                         #(provenance/require-current-certified-literal-route-target-resolver!
                           resolver source)))
    (with-redefs [application/operation-summary (fn [& _] {})]
      (assert-rejection! :substituted-certification-or-route-checker
                         #(issue!))
      (assert-rejection! :substituted-certification-or-route-checker
                         #(resolver claim-request)))
    (is (= :example/claim (:operation-id (resolver claim-request))))))


(deftest declared-command-operations-must-agree-with-both-current-certified-plans
  ;; Both application operations in the real fixture deliberately use the
  ;; SAME physical command operation. The route ID and application operation
  ;; name must not become a substitute source of command semantics.
  (let [contract (checked-source)
        current-commands
        (into {} (for [op [multi/claim multi/cancel]]
                   [op (get-in contract
                               [:certified-operations op :current-source
                                :command-operation])]))]
    (is (= {multi/claim :claim multi/cancel :claim} current-commands))
    (is (not= multi/cancel (:command-operation
                            (get declarations multi/cancel-route))))
    (doseq [route [multi/claim-route multi/cancel-route]
            wrong [:cancel :create :delete :update :unrelated-operation]]
      (testing (str "certified-plan command operation at " route
                    " cannot be overridden by " wrong)
        ;; All of these declarations are well-formed maps with well-formed
        ;; targets. They must reach the NEW semantic disagreement guard,
        ;; rather than being rejected by an earlier shape/type check.
        (let [candidate (assoc-in declarations [route :command-operation]
                                  wrong)]
          (is (= (get-in declarations [route :target])
                 (get-in candidate [route :target])))
          (assert-rejection!
           :declared-command-operation-disagrees-with-certified-plan
           #(issue! candidate)))))
    ;; A legitimate declaration still resolves to the actual plan's
    ;; command identity independently of request parameters.
    (let [resolver (issued!)]
      (doseq [[request app-operation] [[claim-request multi/claim]
                                       [cancel-request multi/cancel]]]
        (is (= app-operation (:operation-id (resolver request))))
        (is (= (get current-commands app-operation)
               (:command-operation (resolver request))))))))

(deftest later-host-redeclaration-cannot-rewrite-an-issued-command-identity
  (let [source (checked-source)
        resolver (issued!)
        original-claim (resolver claim-request)
        original-cancel (resolver cancel-request)
        wrong-claim (assoc-in declarations
                              [multi/claim-route :command-operation] :cancel)
        wrong-cancel (assoc-in declarations
                               [multi/cancel-route :command-operation] :cancel)]
    ;; An immutable replacement of the application's declaration does not
    ;; alter an already-issued, source-checked resolver; the replacement
    ;; cannot obtain a new issued resolver either.
    (assert-rejection!
     :declared-command-operation-disagrees-with-certified-plan
     #(issue! wrong-claim))
    (assert-rejection!
     :declared-command-operation-disagrees-with-certified-plan
     #(issue! wrong-cancel))
    (is (= original-claim (resolver claim-request)))
    (is (= original-cancel (resolver cancel-request)))
    (is (= :joined-to-current-certified-plan-source
           (:command-operation-identity
            (provenance/require-current-certified-literal-route-target-resolver!
             resolver source))))
    (is (= [:claims private-claim-id] (:target (resolver claim-request))))
    (is (= [:claims private-cancel-id] (:target (resolver cancel-request))))
    ;; Fixed document identities are still independent application premises;
    ;; successfully matching command operations cannot certify their truth.
    (let [report (provenance/require-current-certified-literal-route-target-resolver!
                  resolver source)]
      (is (= :application-premise-not-proved
             (:static-declaration-authenticity report)))
      (is (= :not-established (:abstract-operation-to-command-refinement report)))
      (is (= :not-established (:physical-transaction-causality report)))
      (is (= :not-issued (:certificate report))))))

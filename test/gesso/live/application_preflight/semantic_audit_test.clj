(ns gesso.live.application-preflight.semantic-audit-test
  "Direct, fixture-light tests of the semantic-audit projection.

   This namespace checks the result of deriving assurance descriptions from a
   report *supplied as already checked*. It cannot establish report currentness,
   artifact integrity, model correctness, or application authority; those are
   checked at the application-preflight coordinator boundary and covered by
   semantic-audit-integration-test."
  (:require
   [clojure.set :as set]
   [clojure.test :refer [deftest is testing]]
   [gesso.live.application-preflight.semantic-audit :as semantic-audit]))

(def ^:private model-premise
  :public-model-operation-contracts-hold)

(def ^:private card-fields
  [:choreography-name
   :browser-role
   :authority-role
   :browser-plan-key
   :browser-plan-digest
   :authority-plan-digest
   :trusted-operation-name
   :command-transport
   :routes
   :execution-capabilities
   :settlement-contract
   :published-change-topics
   :affected-scopes
   :affected-fragments
   :authoritative-acquisitions
   :rendered-affordances])

(def ^:private claim-summary
  {:choreography-name :request/lifecycle
   :browser-role :helper
   :authority-role :server
   :browser-plan-key :claim
   :browser-plan-digest "browser-claim-digest"
   :authority-plan-digest "authority-claim-digest"
   :trusted-operation-name 'request-model/claim!
   :command-transport :htmx-post
   :routes [{:method :post :path "/requests/:id/claim"}]
   :execution-capabilities #{:execute/claim}
   :settlement-contract {:success :committed :failure :rejected}
   :published-change-topics #{:request/change}
   :affected-scopes #{[:request :id]}
   :affected-fragments #{:request-card}
   :authoritative-acquisitions [{:method :get :path "/requests/:id"}]
   :rendered-affordances [{:surface :request-board :path [0 1]}]
   ;; Deliberately excluded from developer-facing operation cards.
   :private-implementation {:handler-object :opaque-host-function}
   :unexpected-policy-assertion {:verified? true}})

(def ^:private cancel-summary
  {:choreography-name :request/lifecycle
   :browser-role :requester
   :authority-role :server
   :trusted-operation-name 'request-model/cancel!
   :command-transport :htmx-post
   :routes [{:method :post :path "/requests/:id/cancel"}]
   :published-change-topics #{:request/change}
   :rendered-affordances []})

(defn- report
  ([operations]
   (report operations #{} []))
  ([operations other-assumptions obligations]
   {:gesso.live.application-preflight/type
    :gesso.live.application-preflight/report
    :gesso.live.application-preflight/version 3
    :valid? true
    :analysis {:operations operations
               :trusted-assumptions (conj other-assumptions model-premise)
               :open-obligations obligations
               ;; Not part of the semantic-audit input contract.
               :private-report-state {:source-product :hidden}}}))

(defn- audit [operations]
  (semantic-audit/audit-from-current-report (report operations)))

(defn- exception-data [f]
  (try
    (f)
    nil
    (catch clojure.lang.ExceptionInfo e
      (ex-data e))))

(deftest successful-empty-report-keeps-policy-coverage-unknown
  (let [result (audit {})]
    (is (= 0 (:operation-count result)))
    (is (empty? (:operation-cards result)))
    (is (empty? (:opaque-model-boundaries result)))
    (is (empty? (:assurance-coverage result)))
    (is (= {:status :not-derivable-from-current-operation-summary
            :count :unknown
            :rules []}
           (:business-rule-catalog result)))
    (is (false? (get-in result [:inspection-scope
                                :complete-business-policy-inventory?])))
    (is (false? (get-in result [:inspection-scope
                                :zero-undetected-policy-escapes-established?])))))

(deftest operation-count-is-derived-from-admitted-summary-keys
  (let [result (audit {:request/claim claim-summary
                       :request/cancel cancel-summary})]
    (is (= 2 (:operation-count result)))
    (is (= #{:request/claim :request/cancel}
           (set (keys (:operation-cards result)))))
    (is (= 2 (count (:opaque-model-boundaries result))))
    (is (= 4 (count (:assurance-coverage result))))))

(deftest operation-card-projects-only-admitted-contract-fields
  (let [card (get-in (audit {:request/claim claim-summary})
                     [:operation-cards :request/claim])]
    (is (= :request/claim (:operation card)))
    (is (= [:analysis :operations :request/claim] (:derived-from card)))
    (is (= (select-keys claim-summary card-fields) (:contract card)))
    (is (= (set card-fields) (set (keys (:contract card)))))
    (is (not (contains? (:contract card) :private-implementation)))
    (is (not (contains? (:contract card) :unexpected-policy-assertion)))
    (is (= :opaque-host-implementation (:model-policy-inspection card)))
    (is (= model-premise (:model-policy-premise card)))))

(deftest absent-optional-contract-fields-are-not-invented
  (let [card (get-in (audit {:request/cancel cancel-summary})
                     [:operation-cards :request/cancel])]
    (is (= (select-keys cancel-summary card-fields) (:contract card)))
    (is (not (contains? (:contract card) :browser-plan-digest)))
    (is (not (contains? (:contract card) :settlement-contract)))
    (is (= [] (get-in card [:contract :rendered-affordances])))))

(deftest operation-cards-remain-operation-local
  (let [result (audit {:request/claim claim-summary
                       :request/cancel cancel-summary})]
    (is (= 'request-model/claim!
           (get-in result [:operation-cards :request/claim
                           :contract :trusted-operation-name])))
    (is (= 'request-model/cancel!
           (get-in result [:operation-cards :request/cancel
                           :contract :trusted-operation-name])))
    (is (= "/requests/:id/claim"
           (get-in result [:operation-cards :request/claim
                           :contract :routes 0 :path])))
    (is (= "/requests/:id/cancel"
           (get-in result [:operation-cards :request/cancel
                           :contract :routes 0 :path])))))

(deftest operation-card-order-does-not-depend-on-map-insertion
  (let [operations-a (array-map :request/z cancel-summary
                                :request/a claim-summary)
        operations-b (array-map :request/a claim-summary
                                :request/z cancel-summary)
        a (audit operations-a)
        b (audit operations-b)]
    (is (= a b))
    (is (= [:request/a :request/z] (vec (keys (:operation-cards a)))))
    (is (= [:request/a :request/z]
           (mapv :operation (:opaque-model-boundaries a))))
    (is (= [[:operation :request/a :runtime-backbone-closure]
            [:operation :request/a :model-implementation-conformance]
            [:operation :request/z :runtime-backbone-closure]
            [:operation :request/z :model-implementation-conformance]]
           (mapv :subject (:assurance-coverage a))))))

(deftest operation-sorting-uses-printable-identity-for-mixed-key-types
  (let [operations (array-map :request/z {} 'request/a {} :request/a {})
        result (audit operations)
        expected (sort-by pr-str (keys operations))]
    (is (= (vec expected) (vec (keys (:operation-cards result)))))
    (is (= (vec expected) (mapv :operation (:opaque-model-boundaries result))))
    (is (= (count operations) (:operation-count result)))))

(deftest each-operation-has-explicit-undischarged-model-boundary
  (let [boundaries (:opaque-model-boundaries
                    (audit {:request/claim claim-summary
                            :request/cancel cancel-summary}))]
    (doseq [{:keys [operation subject trusted-operation-name premise
                    class why-opaque source-of-evidence discharged?]}
            boundaries]
      (testing (str "model conformance: " operation)
        (is (= [:operation operation :model-implementation-conformance]
               subject))
        (is (= (get {:request/claim 'request-model/claim!
                     :request/cancel 'request-model/cancel!}
                    operation)
               trusted-operation-name))
        (is (= model-premise premise))
        (is (= :trusted class))
        (is (= :host-model-predicates-and-transaction-guards-not-in-operation-summary
               why-opaque))
        (is (= :trusted-application-model-contract source-of-evidence))
        (is (false? discharged?))))))

(deftest coverage-separates-runtime-assembly-from-model-semantics
  (let [coverage (:assurance-coverage (audit {:request/claim claim-summary}))]
    (is (= 2 (count coverage)))
    (is (= {:subject [:operation :request/claim :runtime-backbone-closure]
            :class :assembly-verified
            :evidence :current-application-preflight-report
            :scope :declared-operation-route-execution-settlement-acquisition-edges}
           (first coverage)))
    (is (= {:subject [:operation :request/claim :model-implementation-conformance]
            :class :trusted
            :premise model-premise
            :scope :opaque-authoritative-model-code}
           (second coverage)))
    (is (not-any? #{:proved :certified}
                  (map :class coverage)))))

(deftest inspection-scope-is-stable-and-non-overclaiming
  (let [scope (:inspection-scope (audit {:request/claim claim-summary}))]
    (is (= :freshly-checked-application-preflight-report (:source scope)))
    (is (= #{:declared-operation-identities
             :operation-runtime-backbone-closure
             :trusted-operation-names
             :declared-routes
             :settlement-and-authoritative-acquisition-relations}
           (:inspected scope)))
    (is (= #{:arbitrary-clojure-model-bodies
             :authorization-and-disclosure-predicates
             :business-rule-meaning-and-exceptions
             :transaction-guard-implementation
             :cross-operation-domain-invariants
             :complete-application-render-enumeration}
           (:not-inspected scope)))
    (is (empty? (set/intersection (:inspected scope)
                                  (:not-inspected scope))))
    (is (false? (:complete-business-policy-inventory? scope)))
    (is (false? (:zero-undetected-policy-escapes-established? scope)))))

(deftest authorization-and-lifecycle-remain-uninspected
  (let [result (audit {:request/claim claim-summary})]
    (is (= {:status :not-inspected
            :reason :opaque-application-model-policy}
           (:authorization-disclosure-view result)))
    (is (= {:status :not-inspected
            :reason :no-admitted-domain-transition-relation-in-this-report}
           (:lifecycle-transition-view result)))
    (is (= {:status :not-inspected
            :reason :no-admitted-rule-dependency-graph-in-this-report}
           (:rule-dependencies result)))))

(deftest additional-trusted-assumptions-are-retained-and-sorted
  (let [extra #{:policy-z :alpha-premise :external-authority}
        result (semantic-audit/audit-from-current-report
                (report {:request/claim claim-summary} extra []))]
    (is (= (vec (sort-by pr-str (conj extra model-premise)))
           (:application-trusted-assumptions result)))
    (is (some #{model-premise} (:application-trusted-assumptions result)))
    (is (= 4 (count (:application-trusted-assumptions result))))))

(deftest open-obligations-are-retained-in-the-supplied-order
  (let [obligations [{:kind :first :status :open}
                     {:kind :second :status :open}]
        result (semantic-audit/audit-from-current-report
                (report {} #{} obligations))]
    (is (= obligations (:open-obligations result)))
    (is (= [:first :second]
           (mapv :kind (:open-obligations result))))
    (is (= [] (:opaque-model-boundaries result)))))

(deftest sequential-obligation-inputs-are-normalized-to-vectors
  (doseq [obligations ['({:kind :open-one} {:kind :open-two})
                       (map identity [{:kind :open-one} {:kind :open-two}])]]
    (let [result (semantic-audit/audit-from-current-report
                  (report {} #{} obligations))]
      (is (vector? (:open-obligations result)))
      (is (= [{:kind :open-one} {:kind :open-two}]
             (:open-obligations result))))))

(deftest independent-source-report-metadata-is-not-exported
  (let [source (assoc (report {:request/claim claim-summary})
                      :secret/source-product {:internal true})
        result (semantic-audit/audit-from-current-report source)]
    (is (= #{:inspection-scope
             :operation-count
             :operation-cards
             :business-rule-catalog
             :authorization-disclosure-view
             :lifecycle-transition-view
             :rule-dependencies
             :opaque-model-boundaries
             :application-trusted-assumptions
             :open-obligations
             :assurance-coverage}
           (set (keys result))))
    (is (not (contains? result :secret/source-product)))
    (is (not (contains? result :private-report-state)))
    (is (not (contains? (get-in result [:operation-cards :request/claim :contract])
                        :private-implementation)))))

(deftest malformed-report-envelopes-are-rejected
  (let [good (report {})
        cases [[:nil nil]
               [:string "report"]
               [:vector []]
               [:empty-map {}]
               [:wrong-type (assoc good :gesso.live.application-preflight/type :foreign/report)]
               [:missing-type (dissoc good :gesso.live.application-preflight/type)]
               [:wrong-version (assoc good :gesso.live.application-preflight/version 4)]
               [:missing-version (dissoc good :gesso.live.application-preflight/version)]
               [:false-validity (assoc good :valid? false)]
               [:nil-validity (assoc good :valid? nil)]
               [:truthy-validity (assoc good :valid? :yes)]
               [:missing-analysis (dissoc good :analysis)]
               [:vector-analysis (assoc good :analysis [])]
               [:missing-operations (update good :analysis dissoc :operations)]
               [:vector-operations (assoc-in good [:analysis :operations] [])]
               [:missing-assumptions (update good :analysis dissoc :trusted-assumptions)]
               [:list-assumptions (assoc-in good [:analysis :trusted-assumptions]
                                            (list model-premise))]
               [:missing-obligations (update good :analysis dissoc :open-obligations)]
               [:map-obligations (assoc-in good [:analysis :open-obligations] {})]]]
    (doseq [[label value] cases]
      (testing (name label)
        (let [data (exception-data
                    #(semantic-audit/audit-from-current-report value))]
          (is (= :gesso.live.application-preflight/error (:error/type data)))
          (is (= :invalid-semantic-audit-input (:error/kind data)))
          (is (= :unsupported-or-unsuccessful-report (:reason data)))
          (is (= value (:value data))))))))

(deftest missing-model-conformance-premise-is-a-distinct-rejection
  (let [good (report {})]
    (doseq [assumptions [#{} #{:unrelated-premise}]]
      (let [bad (assoc-in good [:analysis :trusted-assumptions] assumptions)
            data (exception-data
                  #(semantic-audit/audit-from-current-report bad))]
        (is (= :invalid-semantic-audit-input (:error/kind data)))
        (is (= :missing-declared-model-conformance-premise (:reason data)))
        (is (= bad (:value data)))))))

(deftest projection-does-not-mutate-its-input
  (let [input (report {:request/claim claim-summary
                       :request/cancel cancel-summary}
                      #{:additional-premise}
                      [{:kind :open}])
        baseline input
        once (semantic-audit/audit-from-current-report input)
        twice (semantic-audit/audit-from-current-report input)]
    (is (= baseline input))
    (is (= once twice))
    (is (= 2 (:operation-count once)))
    (is (= [{:kind :open}] (:open-obligations once)))))

(deftest equivalent-input-map-orders-have-identical-audits
  (let [a (report (array-map :request/cancel cancel-summary
                             :request/claim claim-summary)
                  #{:beta :alpha}
                  [{:kind :open}])
        b (report (array-map :request/claim claim-summary
                             :request/cancel cancel-summary)
                  #{:alpha :beta}
                  [{:kind :open}])]
    (is (= (semantic-audit/audit-from-current-report a)
           (semantic-audit/audit-from-current-report b)))
    (is (= a b))))

(deftest report-shape-check-is-not-a-substitute-for-coordinator-authority
  ;; This pure projection only recognizes shape and declared premise presence.
  ;; It has no original application assembly or browser artifact to authenticate.
  (let [synthetic (report {:request/claim claim-summary})
        result (semantic-audit/audit-from-current-report synthetic)]
    (is (map? result))
    (is (= :freshly-checked-application-preflight-report
           (get-in result [:inspection-scope :source])))
    (is (= :trusted
           (:class (first (:opaque-model-boundaries result)))))
    (is (false? (:discharged? (first (:opaque-model-boundaries result)))))))

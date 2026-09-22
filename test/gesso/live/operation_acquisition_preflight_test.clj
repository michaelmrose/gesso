(ns gesso.live.operation-acquisition-preflight-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.choreo.identity :as identity]
   [gesso.choreo.preflight :as choreo-preflight]
   [gesso.live.acquisition-preflight :as acquisition]
   [gesso.live.browser.preflight :as browser-preflight]
   [gesso.live.model :as model]
   [gesso.live.operation-acquisition-preflight :as operation-acquisition]
   [gesso.live.optimistic.capability :as capability]
   [gesso.live.optimistic.choreo :as optimistic-choreo]
   [gesso.live.optimistic.execution-preflight :as execution-preflight]
   [gesso.live.optimistic.preflight :as operation-preflight]
   [gesso.live.optimistic.route-preflight :as route-preflight]
   [gesso.live.optimistic.server :as server]))

;; =============================================================================
;; Helpers
;; =============================================================================

(defn- error-data
  [f]
  (try
    (f)
    nil
    (catch Throwable error
      (ex-data error))))

(defn- error-kind
  [f]
  (:error/kind (error-data f)))

(defn- error-kinds
  [report]
  (set (map :kind (:errors report))))

(defn- warning-kinds
  [report]
  (set (map :kind (:warnings report))))

(defn- issue-of-kind
  [issues kind]
  (first (filter #(= kind (:kind %)) issues)))

(defn- allow?
  [_ctx _id]
  true)

(defn- html-response
  [node]
  {:status 200
   :headers {"content-type" "text/html; charset=utf-8"}
   :body node})

(defn- generic-query
  [_ctx id]
  {:fragment/id (str id)})

(defn- generic-render
  [{:keys [fragment/id]}]
  [:section {:id id}])

(defn- compiled-live
  "Compile a fixture with two managed Request projections and one graph-reachable
   audit scope that deliberately has no fragment.

   The no-fragment scope is important: semantic impact must remain visible even
   when there is no managed projection and therefore no acquisition obligation."
  []
  (model/compile-live-app
   {:response html-response

    :scopes
    {:request-toolbar
     {:topic :fixture/request-toolbar
      :id-key :request/location-id
      :authorized? allow?}

     :request-list
     {:topic :fixture/request-list
      :id-key :request/location-id
      :authorized? allow?}

     :audit-scope
     {:topic :fixture/audit
      :id-key :request/location-id
      :authorized? allow?}}

    :graph
    {:request
     [{:scope :request-toolbar
       :id-key :request/location-id}
      {:scope :request-list
       :id-key :request/location-id}]

     :request-assignment
     [{:scope :request-list
       :id-key :request/location-id}]

     :audit
     [{:scope :audit-scope
       :id-key :request/location-id}]}

    :fragments
    {:request-toolbar
     {:scope :request-toolbar
      :id-fn (fn [id] (str "request-toolbar-" id))
      :query generic-query
      :render generic-render
      :swap :outerHTML}

     :request-list
     {:scope :request-list
      :id-fn (fn [id] (str "request-list-" id))
      :query generic-query
      :render generic-render
      :swap :outerHTML}}}))

(defn- realization
  [live-app fragment]
  (acquisition/require-acquisition-realization!
   {:name (keyword "fixture" (str (name fragment) "-acquisition"))
    :live-app live-app
    :fragment fragment
    :fragment-route
    (acquisition/fragment-route
     {:fragment fragment
      :path (str "/fragments/" (name fragment))})
    :stream-route
    (acquisition/stream-route
     {:fragment fragment
      :path (str "/streams/" (name fragment))})}))

(defn- acquisition-assembly
  ([live-app]
   (acquisition-assembly live-app {}))
  ([live-app overrides]
   (acquisition/require-acquisition-assembly!
    (merge
     {:name :fixture/live-acquisition
      :live-app live-app
      :realizations
      {:request-toolbar (realization live-app :request-toolbar)
       :request-list (realization live-app :request-list)}}
     overrides))))

(def trusted-principal
  (identity/principal "helper-1"))

(defn- trusted-operation
  ([operation]
   (trusted-operation operation {}))
  ([operation overrides]
   (server/operation
    (merge
     {:name (keyword (namespace operation)
                     (str (name operation) "-optimistic"))
      :operation operation
      :browser-role :browser
      :authority-role :authority
      :execute!
      (fn [_]
        {:resolution :rejected
         :reason :test-only})}
     overrides))))

(defn- browser-plan
  [prepared-operation]
  (optimistic-choreo/command-plan
   {:name (:name prepared-operation)
    :operation (:operation prepared-operation)
    :browser-role (:browser-role prepared-operation)
    :authority-role (:authority-role prepared-operation)}
   :browser))

(defn- browser-assembly
  [operations]
  (let [plans
        (into
         (sorted-map)
         (map (fn [[operation entry]]
                [operation (browser-plan entry)]))
         operations)
        plan-registry
        (choreo-preflight/require-plan-registry!
         {:name :fixture/operation-acquisition-plans
          :plans plans
          :required-keys (set (keys plans))
          :single-role? true
          :expected-role :browser})]
    (browser-preflight/require-browser-assembly!
     {:name :fixture/operation-acquisition-browser
      :plan-registry plan-registry
      :browser-role :browser
      :required-plan-keys (set (keys plans))
      :optimistic? true
      :optimistic-htmx? true})))

(defn- operation-assembly
  [operations]
  (let [browser (browser-assembly operations)
        capabilities
        (capability/operation-capabilities
         (get-in browser [:plan-registry :plans]))]
    (operation-preflight/require-operation-assembly!
     {:name :fixture/operation-acquisition-operations
      :browser-assembly browser
      :operation-capabilities capabilities
      :server-operations operations})))

(defn- route-capabilities
  [operations]
  (into
   (sorted-map)
   (map
    (fn [operation]
      [operation
       (route-preflight/route-capability
        {:operation operation
         :method :post
         :path (str "/operations/" (namespace operation) "/" (name operation))
         :transports #{:htmx}})]))
   (keys operations)))

(defn- execution-assembly
  [operations]
  (let [operation-assembly' (operation-assembly operations)
        route-assembly'
        (route-preflight/require-route-assembly!
         {:name :fixture/operation-acquisition-routes
          :operation-assembly operation-assembly'
          :route-capabilities (route-capabilities operations)})
        prepared-server
        (server/server
         {:principal-fn (fn [_] trusted-principal)
          :operations operations})]
    (execution-preflight/require-execution-assembly!
     {:name :fixture/operation-acquisition-execution
      :route-assembly route-assembly'
      :server prepared-server})))

(defn- closed-fixture
  ([operations]
   (closed-fixture operations {}))
  ([operations {:keys [live-app acquisition-overrides name]
                :or {name :fixture/operation-acquisition}}]
   (let [live-app' (or live-app (compiled-live))
         execution (execution-assembly operations)
         acquisition' (acquisition-assembly live-app' acquisition-overrides)
         options {:name name
                  :execution-assembly execution
                  :acquisition-assembly acquisition'}]
     {:live-app live-app'
      :execution-assembly execution
      :acquisition-assembly acquisition'
      :options options
      :report (operation-acquisition/check-operation-acquisition options)})))

(defn- standard-operations
  []
  {:request/claim
   (trusted-operation
    :request/claim
    {:published-change-topics #{:request}})

   :request/reassign
   (trusted-operation
    :request/reassign
    {:published-change-topics
     #{:request :request-assignment :audit :external/audit}})

   :request/cancel
   (trusted-operation
    :request/cancel
    {:published-change-topics #{}})})

;; =============================================================================
;; Canonical closure and derived impact
;; =============================================================================

(deftest successful-join-derives-operation-impact-without-an-operation-fragment-registry
  (let [{:keys [options report execution-assembly acquisition-assembly]}
        (closed-fixture (standard-operations))
        assembly
        (operation-acquisition/require-operation-acquisition-assembly! options)
        affected-scopes
        (operation-acquisition/affected-scopes assembly)
        affected-fragments
        (operation-acquisition/affected-fragments assembly)
        explanation
        (operation-acquisition/explain assembly)]
    (testing "the report is a current closed derivation"
      (is (operation-acquisition/report? report))
      (is (operation-acquisition/valid? report))
      (is (= [] (:errors report)))
      (is (= #{:request/claim :request/reassign :request/cancel}
             (get-in report [:analysis :operations]))))

    (testing "claim derives its two managed projections from :request"
      (is (= #{:request-toolbar :request-list}
             (get affected-scopes :request/claim)))
      (is (= #{:request-toolbar :request-list}
             (get affected-fragments :request/claim))))

    (testing "multi-topic publication keeps semantic scope impact distinct from projected fragments"
      (is (= #{:request-toolbar :request-list :audit-scope}
             (get affected-scopes :request/reassign)))
      (is (= #{:request-toolbar :request-list}
             (get affected-fragments :request/reassign))))

    (testing "explicit empty publication closes to empty impact"
      (is (= #{} (get affected-scopes :request/cancel)))
      (is (= #{} (get affected-fragments :request/cancel))))

    (testing "the assembly stores only the exact upstream closed products"
      (is (operation-acquisition/operation-acquisition-assembly? assembly))
      (is (identical? execution-assembly (:execution-assembly assembly)))
      (is (identical? acquisition-assembly (:acquisition-assembly assembly)))
      (is (= #{:gesso.live.operation-acquisition-preflight/type
               :gesso.live.operation-acquisition-preflight/version
               :name
               :execution-assembly
               :acquisition-assembly}
             (set (keys assembly))))
      (is (not (contains? assembly :affected-scopes)))
      (is (not (contains? assembly :affected-fragments)))
      (is (not (contains? assembly :published-change-topics))))

    (testing "explain agrees with the derived public accessors"
      (is (= affected-scopes (:affected-scopes explanation)))
      (is (= affected-fragments (:affected-fragments explanation)))
      (is (= (execution-preflight/published-change-topics execution-assembly)
             (:published-change-topics explanation)))
      (is (= :operation-publication-to-authoritative-acquisition-preflight-closed
             (:guarantee explanation))))))

(deftest multiple-operations-may-share-one-publication-topic
  (let [operations
        {:request/claim
         (trusted-operation :request/claim
                            {:published-change-topics #{:request}})
         :request/cancel
         (trusted-operation :request/cancel
                            {:published-change-topics #{:request}})}
        {:keys [options]}
        (closed-fixture operations)
        assembly
        (operation-acquisition/require-operation-acquisition-assembly! options)]
    (is (= {:request/claim #{:request-toolbar :request-list}
            :request/cancel #{:request-toolbar :request-list}}
           (operation-acquisition/affected-scopes assembly)))
    (is (= {:request/claim #{:request-toolbar :request-list}
            :request/cancel #{:request-toolbar :request-list}}
           (operation-acquisition/affected-fragments assembly)))))

(deftest graph-reachable-scope-with-no-projected-fragment-remains-visible
  (let [operations
        {:request/audit
         (trusted-operation
          :request/audit
          {:published-change-topics #{:audit}})}
        {:keys [options report]}
        (closed-fixture operations)
        assembly
        (operation-acquisition/require-operation-acquisition-assembly! options)]
    (is (operation-acquisition/valid? report))
    (is (= #{:audit-scope}
           (get (operation-acquisition/affected-scopes assembly)
                :request/audit)))
    (is (= #{}
           (get (operation-acquisition/affected-fragments assembly)
                :request/audit)))
    (is (= [] (:warnings report))
        "The topic is consumed by the compiled Live graph even though no fragment projects its scope.")))

(deftest one-operation-may-publish-several-known-topics
  (let [operations
        {:request/reassign
         (trusted-operation
          :request/reassign
          {:published-change-topics
           #{:request :request-assignment :audit}})}
        {:keys [options]}
        (closed-fixture operations)
        assembly
        (operation-acquisition/require-operation-acquisition-assembly! options)]
    (is (= #{:request-toolbar :request-list :audit-scope}
           (get (operation-acquisition/affected-scopes assembly)
                :request/reassign)))
    (is (= #{:request-toolbar :request-list}
           (get (operation-acquisition/affected-fragments assembly)
                :request/reassign)))))

;; =============================================================================
;; Publication closure
;; =============================================================================

(deftest undeclared-route-exposed-operation-publication-fails-closed
  (let [operations
        {:request/claim
         (trusted-operation :request/claim)}
        {:keys [options report]}
        (closed-fixture operations)
        error
        (issue-of-kind (:errors report) :undeclared-operation-publication)]
    (is (operation-acquisition/report? report))
    (is (not (operation-acquisition/valid? report)))
    (is (= #{:undeclared-operation-publication}
           (error-kinds report)))
    (is (= :request/claim (:operation error)))
    (is (nil? (:published-change-topics error)))
    (is (= :operation-acquisition-preflight-failed
           (error-kind
            #(operation-acquisition/require-operation-acquisition-assembly!
              options))))))

(deftest explicit-empty-publication-is-closed-not-missing
  (let [operations
        {:request/no-live-effect
         (trusted-operation
          :request/no-live-effect
          {:published-change-topics #{}})}
        {:keys [options report]}
        (closed-fixture operations)
        assembly
        (operation-acquisition/require-operation-acquisition-assembly! options)]
    (is (operation-acquisition/valid? report))
    (is (= {:request/no-live-effect #{}}
           (get-in report [:analysis :published-change-topics-by-operation])))
    (is (= {:request/no-live-effect #{}}
           (operation-acquisition/affected-scopes assembly)))
    (is (= {:request/no-live-effect #{}}
           (operation-acquisition/affected-fragments assembly)))))

(deftest published-topic-absent-from-this-live-app-is-warning-not-fatal
  (let [operations
        {:request/claim
         (trusted-operation
          :request/claim
          {:published-change-topics #{:request :external/audit}})}
        {:keys [options report]}
        (closed-fixture operations)
        assembly
        (operation-acquisition/require-operation-acquisition-assembly! options)
        warning
        (issue-of-kind
         (:warnings report)
         :published-change-topic-not-consumed-by-live-app)]
    (is (operation-acquisition/valid? report))
    (is (= #{:published-change-topic-not-consumed-by-live-app}
           (warning-kinds report)))
    (is (= :request/claim (:operation warning)))
    (is (= #{:external/audit} (:topics warning)))
    (is (= #{:request :request-assignment :audit}
           (:known-live-change-topics warning)))
    (is (= #{:request-toolbar :request-list}
           (get (operation-acquisition/affected-fragments assembly)
                :request/claim)))))

;; =============================================================================
;; Upstream assembly correspondence
;; =============================================================================

(deftest cross-live-app-acquisition-assembly-remains-an-explicit-pairing
  (let [operations
        {:request/claim
         (trusted-operation
          :request/claim
          {:published-change-topics #{:request}})}
        live-a (compiled-live)
        live-b
        (model/compile-live-app
         {:response html-response
          :scopes
          {:other
           {:topic :fixture/other
            :id-key :entity/id
            :authorized? allow?}}
          :graph
          {:other-change
           [{:scope :other :id-key :entity/id}]}
          :fragments
          {:other
           {:scope :other
            :id-fn (fn [id] (str "other-" id))
            :query generic-query
            :render generic-render
            :swap :outerHTML}}})
        execution (execution-assembly operations)
        acquisition-b
        (acquisition/require-acquisition-assembly!
         {:name :fixture/other-acquisition
          :live-app live-b
          :realizations {:other (realization live-b :other)}})
        options
        {:name :fixture/cross-live-pair
         :execution-assembly execution
         :acquisition-assembly acquisition-b}
        report
        (operation-acquisition/check-operation-acquisition options)
        assembly
        (operation-acquisition/require-operation-acquisition-assembly! options)]
    (testing "the join does not invent a hidden application identity coupling"
      (is (operation-acquisition/valid? report))
      (is (= #{}
             (get (operation-acquisition/affected-scopes assembly)
                  :request/claim)))
      (is (= #{}
             (get (operation-acquisition/affected-fragments assembly)
                  :request/claim)))
      (is (= #{:published-change-topic-not-consumed-by-live-app}
             (warning-kinds report))))

    (testing "the unused same-shape Live app does not influence the result"
      (is (not= live-a (:live-app acquisition-b))))))

(deftest invalid-upstream-assemblies-are-rejected-before-impact-is-claimed
  (let [{:keys [execution-assembly acquisition-assembly]}
        (closed-fixture (standard-operations))
        bad-execution
        (assoc execution-assembly :name "not-a-keyword")
        bad-acquisition
        (assoc acquisition-assembly :name "not-a-keyword")
        execution-report
        (operation-acquisition/check-operation-acquisition
         {:execution-assembly bad-execution
          :acquisition-assembly acquisition-assembly})
        acquisition-report
        (operation-acquisition/check-operation-acquisition
         {:execution-assembly execution-assembly
          :acquisition-assembly bad-acquisition})]
    (is (= #{:invalid-execution-assembly}
           (error-kinds execution-report)))
    (is (= {}
           (get-in execution-report
                   [:analysis :affected-fragments-by-operation])))
    (is (= #{:invalid-acquisition-assembly}
           (error-kinds acquisition-report)))
    (is (= {}
           (get-in acquisition-report
                   [:analysis :affected-fragments-by-operation])))))

;; =============================================================================
;; Closed report / assembly recognizers
;; =============================================================================

(deftest forged-positive-report-is-not-recognized
  (let [{:keys [report]}
        (closed-fixture (standard-operations))
        forged
        (assoc report :valid? true :errors [])
        forged-impact
        (assoc-in
         report
         [:analysis :affected-fragments-by-operation :request/claim]
         #{:request-list})]
    (is (operation-acquisition/report? report))
    (is (not (operation-acquisition/report? forged-impact)))
    ;; The original report is already valid, so simply forcing :valid? true is a
    ;; no-op rather than evidence of recognizer weakness.
    (is (= report forged))))

(deftest failed-report-cannot-be-forged-positive-by-clearing-errors
  (let [operations
        {:request/claim (trusted-operation :request/claim)}
        {:keys [report]}
        (closed-fixture operations)
        forged
        (assoc report :valid? true :errors [])]
    (is (operation-acquisition/report? report))
    (is (not (operation-acquisition/valid? report)))
    (is (not (operation-acquisition/report? forged)))
    (is (not (operation-acquisition/valid? forged)))))

(deftest derived-impact-tampering-invalidates-report
  (let [{:keys [report]}
        (closed-fixture (standard-operations))]
    (doseq [tampered
            [(assoc-in
              report
              [:analysis :affected-scopes-by-operation :request/claim]
              #{:request-list})
             (assoc-in
              report
              [:analysis :affected-fragments-by-operation :request/claim]
              #{:request-list})
             (assoc-in
              report
              [:analysis :published-change-topics-by-operation :request/claim]
              #{:audit})
             (assoc-in
              report
              [:analysis :known-live-change-topics]
              #{:request})]]
      (is (not (operation-acquisition/report? tampered))))))

(deftest closed-assembly-rejects-duplicated-derived-registries
  (let [{:keys [options]}
        (closed-fixture (standard-operations))
        assembly
        (operation-acquisition/require-operation-acquisition-assembly! options)]
    (doseq [tampered
            [(assoc assembly
                    :affected-fragments
                    (operation-acquisition/affected-fragments assembly))
             (assoc assembly
                    :affected-scopes
                    (operation-acquisition/affected-scopes assembly))
             (assoc assembly
                    :published-change-topics
                    (execution-preflight/published-change-topics
                     (:execution-assembly assembly)))]]
      (is (not (operation-acquisition/operation-acquisition-assembly?
                tampered)))
      (is (= :invalid-operation-acquisition-assembly
             (error-kind
              #(operation-acquisition/affected-fragments tampered)))))))

(deftest tampering-either-embedded-upstream-assembly-invalidates-the-closed-product
  (let [{:keys [options]}
        (closed-fixture (standard-operations))
        assembly
        (operation-acquisition/require-operation-acquisition-assembly! options)
        bad-execution
        (assoc (:execution-assembly assembly)
               :name "not-a-keyword")
        bad-acquisition
        (assoc (:acquisition-assembly assembly)
               :name "not-a-keyword")]
    (is (not
         (operation-acquisition/operation-acquisition-assembly?
          (assoc assembly :execution-assembly bad-execution))))
    (is (not
         (operation-acquisition/operation-acquisition-assembly?
          (assoc assembly :acquisition-assembly bad-acquisition))))))

(deftest changing-one-valid-trusted-publication-declaration-produces-a-new-valid-assembly
  (let [base-operations
        {:request/claim
         (trusted-operation
          :request/claim
          {:published-change-topics #{:request}})}
        other-operations
        {:request/claim
         (trusted-operation
          :request/claim
          {:published-change-topics #{:audit}})}
        base
        (closed-fixture base-operations)
        other
        (closed-fixture other-operations)
        base-assembly
        (operation-acquisition/require-operation-acquisition-assembly!
         (:options base))
        other-assembly
        (operation-acquisition/require-operation-acquisition-assembly!
         (:options other))]
    (testing "this layer verifies correspondence, not truth of the trusted declaration itself"
      (is (operation-acquisition/operation-acquisition-assembly? base-assembly))
      (is (operation-acquisition/operation-acquisition-assembly? other-assembly))
      (is (= #{:request-toolbar :request-list}
             (get (operation-acquisition/affected-fragments base-assembly)
                  :request/claim)))
      (is (= #{}
             (get (operation-acquisition/affected-fragments other-assembly)
                  :request/claim)))
      (is (= #{:audit-scope}
             (get (operation-acquisition/affected-scopes other-assembly)
                  :request/claim))))))

;; =============================================================================
;; Option / explanation closure
;; =============================================================================

(deftest option-shape-fails-early-and-locally
  (let [{:keys [execution-assembly acquisition-assembly]}
        (closed-fixture (standard-operations))]
    (is (= :missing-option
           (error-kind
            #(operation-acquisition/check-operation-acquisition
              {:execution-assembly execution-assembly}))))
    (is (= :unknown-option-keys
           (error-kind
            #(operation-acquisition/check-operation-acquisition
              {:execution-assembly execution-assembly
               :acquisition-assembly acquisition-assembly
               :operation-fragments {}}))))
    (is (= :invalid-name
           (error-kind
            #(operation-acquisition/check-operation-acquisition
              {:name "not-a-keyword"
               :execution-assembly execution-assembly
               :acquisition-assembly acquisition-assembly}))))))

(deftest explain-rejects-unrecognized-or-tampered-values
  (let [{:keys [options report]}
        (closed-fixture (standard-operations))
        assembly
        (operation-acquisition/require-operation-acquisition-assembly! options)
        bad-report
        (assoc-in report
                  [:analysis :affected-fragments-by-operation :request/claim]
                  #{})
        bad-assembly
        (assoc assembly :name "not-a-keyword")]
    (is (= :unrecognized-value
           (error-kind #(operation-acquisition/explain bad-report))))
    (is (= :unrecognized-value
           (error-kind #(operation-acquisition/explain bad-assembly))))
    (is (= :unrecognized-value
           (error-kind #(operation-acquisition/explain {:valid? true}))))))

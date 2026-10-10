(ns gesso.live.application-preflight.operation-summary-test
  "Direct tests for source-derived operation summaries.

   An OperationAcquisitionAssembly is recognized only by the upstream checker;
   this suite replaces that checker with controlled reports to exercise this
   module's boundary without assembling a browser artifact or application.
   These tests establish projection, call ordering, diagnostics, and rejection
   behavior. They do not establish the correctness of upstream products,
   application authorization, or opaque model policy."
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.live.application-preflight.operation-summary :as summary]
   [gesso.live.operation-acquisition-preflight :as operation-acquisition]))

(def ^:private claim-fragment-route
  {:method :get
   :path "/fragments/request-list"
   :boundary :gesso.live.core/render-fragment-response
   :extra-field :not-exported})

(def ^:private claim-stream-route
  {:method :get
   :path "/streams/request-list"
   :boundary :gesso.live.transport.sse/start-fragment-stream!
   :extra-field :not-exported})

(def ^:private fixture-browser-assembly
  {:fixture/browser :already-admitted})

(def ^:private claim-realization
  {:choreography-name :request/claim-flow
   :browser-role :browser
   :authority-role :authority
   :plan-key :plan/claim
   :browser-plan-digest "browser-claim"
   :authority-plan-digest "authority-claim"})

(def ^:private cancel-realization
  {:choreography-name :request/cancel-flow
   :browser-role :browser
   :authority-role :authority
   :plan-key :plan/cancel
   :browser-plan-digest "browser-cancel"
   :authority-plan-digest "authority-cancel"})

(def ^:private fixture
  {:gesso.live.operation-acquisition-preflight/type
   operation-acquisition/operation-acquisition-assembly-type
   :gesso.live.operation-acquisition-preflight/version
   operation-acquisition/preflight-version
   :name :fixture/operation-summary
   :execution-assembly
   {:route-assembly
    {:operation-assembly
     {:browser-assembly fixture-browser-assembly
      :operations {:request/cancel cancel-realization
                   :request/claim claim-realization}
      :route-requirements
      {:request/claim {:transport :htmx}
       :request/cancel {:transport :htmx}}}
     :routes
     {:route/z-claim {:operation :request/claim
                      :method :post :path "/requests/:id/claim"
                      :required-transport :htmx}
      :route/a-claim {:operation :request/claim
                      :method :post :path "/requests/:request-id/claim"
                      :required-transport :htmx}
      :route/cancel {:operation :request/cancel
                     :method :post :path "/requests/:id/cancel"
                     :required-transport :htmx}}}
    :server {:operations
             {:request/claim {:name :trusted/claim}
              :request/cancel {:name :trusted/cancel}}}
    :execution-capabilities
    {:request/claim {:fixture/capability :claim}
     :request/cancel {:fixture/capability :cancel}}
    :settlement-contracts
    {:request/claim {:fixture/settlement :claim}
     :request/cancel {:fixture/settlement :cancel}}}
   :acquisition-assembly
   {:realizations
    {:request-list
     {:scope :request-list
      :fragment-route claim-fragment-route
      :stream-route claim-stream-route
      :private-entry :must-not-escape}
     :request-toolbar
     {:scope :request-toolbar
      :fragment-route {:method :get :path "/fragments/toolbar"
                       :boundary :fragment/render}
      :stream-route {:method :get :path "/streams/toolbar"
                     :boundary :stream/start}}}}})

(def ^:private current-report
  {:valid? true
   :analysis
   {:published-change-topics-by-operation
    {:request/claim #{:request :external/audit}
     :request/cancel #{}}
    :affected-scopes-by-operation
    {:request/claim #{:request-list :request-toolbar :audit-scope}
     :request/cancel #{}}
    :affected-fragments-by-operation
    {:request/claim #{:request-list :request-toolbar}
     :request/cancel #{}}}})

(defn- error
  [f]
  (try
    (f)
    nil
    (catch clojure.lang.ExceptionInfo e
      e)))

(defn- checked-summary
  ([] (checked-summary fixture current-report))
  ([assembly report]
   (with-redefs [operation-acquisition/check-operation-acquisition
                 (fn [_] report)]
     (summary/operation-summary assembly))))

(deftest closed-operation-acquisition-shape-is-recognized
  (is (true? (summary/operation-acquisition-assembly-shape? fixture)))
  (is (true? (summary/operation-acquisition-assembly-shape?
              (assoc fixture :name nil))))
  (is (true? (summary/operation-acquisition-assembly-shape?
              (assoc fixture :name :other/current)))))

(deftest operation-acquisition-shape-rejects-unexpected-and-missing-keys
  (doseq [invalid [(dissoc fixture :name)
                   (dissoc fixture :execution-assembly)
                   (dissoc fixture :acquisition-assembly)
                   (assoc fixture :extra/authority true)
                   (assoc fixture :operations {})
                   (assoc fixture :analysis {:fabricated true})]]
    (testing (pr-str (set (keys invalid)))
      (is (false? (summary/operation-acquisition-assembly-shape? invalid))))))

(deftest shape-rejects-invalid-identity-and-version
  (doseq [invalid [(assoc fixture :gesso.live.operation-acquisition-preflight/type
                               :fixture/forgery)
                   (assoc fixture :gesso.live.operation-acquisition-preflight/version 999)
                   (assoc fixture :gesso.live.operation-acquisition-preflight/version nil)
                   (assoc fixture :name "text")
                   (assoc fixture :name 42)
                   (assoc fixture :name [])]]
    (is (false? (summary/operation-acquisition-assembly-shape? invalid)))))

(deftest shape-is-not-repaired-from-arbitrary-values
  (doseq [invalid [nil 0 true [] #{} "assembly" :assembly]]
    (is (false? (summary/operation-acquisition-assembly-shape? invalid)))))

(deftest browser-assembly-is-derived-from-nested-source
  (is (= fixture-browser-assembly (summary/browser-assembly fixture)))
  (is (= {:fixture/browser :replacement}
         (summary/browser-assembly
          (assoc-in fixture
                    [:execution-assembly :route-assembly
                     :operation-assembly :browser-assembly]
                    {:fixture/browser :replacement}))))
  (is (nil? (summary/browser-assembly {})))
  (is (nil? (summary/browser-assembly nil))))

(deftest current-report-checks-exact-admitted-input-coordinates
  (let [calls (atom [])]
    (with-redefs [operation-acquisition/check-operation-acquisition
                  (fn [options]
                    (swap! calls conj options)
                    current-report)]
      (is (identical? current-report
                      (summary/current-operation-acquisition-report fixture)))
      (is (= [{:name :fixture/operation-summary
               :execution-assembly (:execution-assembly fixture)
               :acquisition-assembly (:acquisition-assembly fixture)}]
             @calls)))))

(deftest current-report-does-not-call-upstream-checker-for-bad-shapes
  (let [calls (atom 0)]
    (with-redefs [operation-acquisition/check-operation-acquisition
                  (fn [_]
                    (swap! calls inc)
                    (throw (Exception. "should not run")))]
      (doseq [bad [nil {} (assoc fixture :unrecognized true)
                   (assoc fixture :name "wrong")]]
        (is (nil? (summary/current-operation-acquisition-report bad))))
      (is (zero? @calls)))))

(deftest current-report-rejects-unaccepted-outcomes
  (doseq [result [nil {} {:valid? false}
                  {:valid? nil} {:valid? 1} {:valid? :yes}
                  {:valid? "true"}]]
    (with-redefs [operation-acquisition/check-operation-acquisition
                  (fn [_] result)]
      (is (nil? (summary/current-operation-acquisition-report fixture))))))

(deftest current-report-does-not-hide-upstream-rejection-as-success
  (doseq [failure [(ex-info "closed" {:error/kind :fixture/closed})
                   (IllegalArgumentException. "invalid")]]
    (with-redefs [operation-acquisition/check-operation-acquisition
                  (fn [_] (throw failure))]
      (is (nil? (summary/current-operation-acquisition-report fixture))))))

(deftest current-report-does-not-cache-previous-success
  (let [counter (atom 0)]
    (with-redefs [operation-acquisition/check-operation-acquisition
                  (fn [_]
                    (if (= 1 (swap! counter inc))
                      current-report
                      {:valid? false}))]
      (is (identical? current-report
                      (summary/current-operation-acquisition-report fixture)))
      (is (nil? (summary/current-operation-acquisition-report fixture)))
      (is (= 2 @counter)))))

(deftest operation-summary-reuses-one-current-report-per-call
  (let [calls (atom 0)]
    (with-redefs [operation-acquisition/check-operation-acquisition
                  (fn [_] (swap! calls inc) current-report)]
      (is (= #{:request/claim :request/cancel}
             (set (keys (summary/operation-summary fixture)))))
      (is (= 1 @calls))
      (summary/operation-summary fixture)
      (is (= 2 @calls)))))

(deftest invalid-assembly-produces-same-application-error-envelope
  (doseq [bad [nil {} (assoc fixture :name "invalid")
               (assoc fixture :gesso.live.operation-acquisition-preflight/version 0)]]
    (let [ex (error #(summary/operation-summary bad))]
      (is (instance? clojure.lang.ExceptionInfo ex))
      (is (= "Application operation summary requires a current OperationAcquisitionAssembly."
             (.getMessage ex)))
      (is (= {:error/type :gesso.live.application-preflight/error
              :error/kind :invalid-operation-acquisition-assembly
              :operation-acquisition-assembly bad}
             (ex-data ex))))))

(deftest rejected-upstream-report-produces-invalid-assembly-error
  (doseq [result [{:valid? false} nil]]
    (with-redefs [operation-acquisition/check-operation-acquisition
                  (fn [_] result)]
      (let [ex (error #(summary/operation-summary fixture))]
        (is (= :invalid-operation-acquisition-assembly
               (:error/kind (ex-data ex))))
        (is (= fixture
               (:operation-acquisition-assembly (ex-data ex))))))))

(deftest upstream-exception-does-not-leak-through-the-public-summary-error
  (with-redefs [operation-acquisition/check-operation-acquisition
                (fn [_] (throw (ex-info "upstream rejected"
                                         {:private/upstream true})))]
    (let [ex (error #(summary/operation-summary fixture))]
      (is (= :gesso.live.application-preflight/error
             (:error/type (ex-data ex))))
      (is (= :invalid-operation-acquisition-assembly
             (:error/kind (ex-data ex))))
      (is (= fixture (:operation-acquisition-assembly (ex-data ex)))))))

(deftest source-summary-uses-closed-operation-set-and-sorted-keys
  (let [result (checked-summary)]
    (is (instance? clojure.lang.Sorted result))
    (is (= [:request/cancel :request/claim] (vec (keys result))))
    (is (not (contains? result :request/undeclared)))
    (is (= #{:request/claim :request/cancel} (set (keys result))))))

(deftest source-summary-preserves-identity-role-plan-and-trusted-name
  (let [entry (:request/claim (checked-summary))]
    (is (= (:choreography-name claim-realization) (:choreography-name entry)))
    (is (= :browser (:browser-role entry)))
    (is (= :authority (:authority-role entry)))
    (is (= :plan/claim (:browser-plan-key entry)))
    (is (= "browser-claim" (:browser-plan-digest entry)))
    (is (= "authority-claim" (:authority-plan-digest entry)))
    (is (= :trusted/claim (:trusted-operation-name entry)))
    (is (= :htmx (:command-transport entry)))
    (is (= {:fixture/capability :claim} (:execution-capabilities entry)))
    (is (= {:fixture/settlement :claim} (:settlement-contract entry)))))

(deftest routes-are-derived-and-sorted-without-inventing-extra-fields
  (let [routes (get-in (checked-summary) [:request/claim :routes])]
    (is (= [{:route-id :route/z-claim
             :method :post :path "/requests/:id/claim"
             :required-transport :htmx}
            {:route-id :route/a-claim
             :method :post :path "/requests/:request-id/claim"
             :required-transport :htmx}]
           routes))
    (is (vector? routes))
    (is (= [:route/cancel]
           (mapv :route-id (get-in (checked-summary)
                                  [:request/cancel :routes]))))))

(deftest publication-and-fragment-effects-come-from-checked-report
  (let [claim (get (checked-summary) :request/claim)]
    (is (= #{:request :external/audit} (:published-change-topics claim)))
    (is (= #{:request-list :request-toolbar :audit-scope}
           (:affected-scopes claim)))
    (is (= #{:request-list :request-toolbar}
           (:affected-fragments claim)))
    (is (not (contains? (:affected-fragments claim) :audit-scope)))))

(deftest empty-publication-does-not-invent-acquisitions
  (let [cancel (:request/cancel (checked-summary))]
    (is (= #{} (:published-change-topics cancel)))
    (is (= #{} (:affected-scopes cancel)))
    (is (= #{} (:affected-fragments cancel)))
    (is (= (sorted-map) (:authoritative-acquisitions cancel)))))

(deftest acquisition-route-export-excludes-private-realization-details
  (let [list-entry (get-in (checked-summary)
                           [:request/claim :authoritative-acquisitions
                            :request-list])]
    (is (= :request-list (:scope list-entry)))
    (is (= (dissoc claim-fragment-route :extra-field)
           (:fragment-route list-entry)))
    (is (= (dissoc claim-stream-route :extra-field)
           (:stream-route list-entry)))
    (is (= #{:scope :fragment-route :stream-route} (set (keys list-entry))))))

(deftest acquisitions-are-keyed-by-affected-fragments-not-all-realizations
  (let [modified (assoc-in current-report
                           [:analysis :affected-fragments-by-operation :request/claim]
                           #{:request-list})
        result (checked-summary fixture modified)]
    (is (= #{:request-list}
           (set (keys (get-in result [:request/claim :authoritative-acquisitions])))))
    (is (= #{:request-list}
           (get-in result [:request/claim :affected-fragments])))))

(deftest missing-acquisition-does-not-create-an-authoritative-route
  (let [modified (assoc-in current-report
                           [:analysis :affected-fragments-by-operation :request/claim]
                           #{:request-list :missing/fragment})
        claim (:request/claim (checked-summary fixture modified))]
    (is (= #{:request-list :missing/fragment}
           (:affected-fragments claim)))
    (is (= #{:request-list}
           (set (keys (:authoritative-acquisitions claim)))))))

(deftest operation-summary-follows-updated-checked-report-and-nested-assembly
  (let [modified (-> fixture
                     (assoc-in [:execution-assembly :server :operations
                                :request/claim :name]
                               :trusted/revised-claim)
                     (assoc-in [:execution-assembly :route-assembly
                                :operation-assembly :operations
                                :request/claim :plan-key]
                               :plan/revised-claim))
        report (assoc-in current-report
                         [:analysis :published-change-topics-by-operation
                          :request/claim] #{:request :revised})
        entry (:request/claim (checked-summary modified report))]
    (is (= :trusted/revised-claim (:trusted-operation-name entry)))
    (is (= :plan/revised-claim (:browser-plan-key entry)))
    (is (= #{:request :revised} (:published-change-topics entry)))))

(deftest derivation-does-not-mutate-supplied-products
  (let [before-fixture fixture
        before-report current-report
        result-a (checked-summary)
        result-b (checked-summary)]
    (is (= before-fixture fixture))
    (is (= before-report current-report))
    (is (= result-a result-b))
    (is (not (contains? result-a :gesso/authority)))))

(deftest no-published-fragment-map-produces-empty-defaults
  (let [modified (assoc current-report :analysis
                        {:published-change-topics-by-operation {}
                         :affected-scopes-by-operation {}
                         :affected-fragments-by-operation {}})
        claim (:request/claim (checked-summary fixture modified))]
    (is (nil? (:published-change-topics claim)))
    (is (= #{} (:affected-scopes claim)))
    (is (= #{} (:affected-fragments claim)))
    (is (= {} (:authoritative-acquisitions claim)))))

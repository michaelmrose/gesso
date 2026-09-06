(ns gesso.choreo.preflight-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.choreo.artifact :as artifact]
   [gesso.choreo.core :as choreo]
   [gesso.choreo.preflight :as preflight]
   [gesso.choreo.project :as project]))

(defn- error-data
  [f]
  (try
    (f)
    nil
    (catch clojure.lang.ExceptionInfo ex
      (ex-data ex))))

(defn- error-kinds
  [report]
  (set (map :kind (:errors report))))

(defn- one-role-plan
  ([role]
   (one-role-plan role :example/prepare))
  ([role action]
   (project/project
    (choreo/->choreography
     {:name :example/preflight-one-role
      :initial :prepare
      :states
      {:prepare
       (choreo/local role action :done)

       :done
       (choreo/return :done)}})
    role)))

(defn- two-role-plans
  []
  (let [choreography
        (choreo/->choreography
         {:name :example/preflight-two-role
          :initial :communicate
          :states
          {:communicate
           (choreo/communicate
            :browser
            :server
            :example/request
            :done
            {:via :http})

           :done
           (choreo/return :done)}})]
    (project/project-all choreography)))

(defn- zeros-digest
  []
  (apply str (repeat 64 "0")))

(deftest successful-preflight-emits-a-closed-portable-plan-registry
  (let [claim-plan
        (one-role-plan :browser :request/claim)

        cancel-plan
        (one-role-plan :browser :request/cancel)

        plans
        {:request/claim claim-plan
         :request/cancel cancel-plan}

        expected-digests
        (into {}
              (map
               (fn [[plan-key plan]]
                 [plan-key
                  (artifact/executable-digest plan)]))
              plans)

        report
        (preflight/check-plan-registry
         {:name :humanhelp/request-browser
          :plans plans
          :required-keys #{:request/claim :request/cancel}
          :single-role? true
          :expected-role :browser
          :expected-digests expected-digests})

        registry
        (preflight/require-plan-registry!
         {:name :humanhelp/request-browser
          :plans plans
          :required-keys #{:request/claim :request/cancel}
          :single-role? true
          :expected-role :browser
          :expected-digests expected-digests})]

    (testing "the preflight report is current, closed, and successful"
      (is (preflight/report? report))
      (is (preflight/valid? report))
      (is (empty? (:errors report)))
      (is (= #{:request/claim :request/cancel}
             (get-in report [:analysis :plan-keys])))
      (is (= #{:browser}
             (get-in report [:analysis :roles])))
      (is (= expected-digests
             (get-in report [:analysis :digests]))))

    (testing "the emitted registry retains only canonical runtime material"
      (is (preflight/plan-registry? registry))
      (is (= :humanhelp/request-browser
             (:name registry)))
      (is (= #{:browser}
             (:roles registry)))
      (is (= plans
             (:plans registry)))
      (is (= expected-digests
             (:digests registry))))

    (testing "the stable explainer distinguishes report and emitted registry"
      (is (= {:type preflight/report-type
              :version preflight/preflight-version
              :valid? true
              :error-count 0
              :warning-count 0
              :name :humanhelp/request-browser
              :plan-count 2
              :roles #{:browser}}
             (preflight/explain report)))
      (is (= 2
             (:plan-count
              (preflight/explain registry)))))))

(deftest required-plan-coverage-is-exact
  (let [plan
        (one-role-plan :browser)

        missing-report
        (preflight/check-plan-registry
         {:plans {:request/claim plan}
          :required-keys #{:request/claim :request/cancel}})

        unexpected-report
        (preflight/check-plan-registry
         {:plans {:request/claim plan
                  :request/debug plan}
          :required-keys #{:request/claim}})]

    (testing "missing logical operations fail before browser emission"
      (is (= #{:missing-required-plans}
             (error-kinds missing-report)))
      (is (= #{:request/cancel}
             (-> missing-report :errors first :missing-keys))))

    (testing "extra generated plans are also drift, not harmless additions"
      (is (= #{:unexpected-plans}
             (error-kinds unexpected-report)))
      (is (= #{:request/debug}
             (-> unexpected-report :errors first :unexpected-keys))))))

(deftest one-physical-runtime-role-is-enforced
  (let [plans
        (two-role-plans)

        report
        (preflight/check-plan-registry
         {:plans {:browser (:browser plans)
                  :server (:server plans)}
          :single-role? true})]
    (is (false? (:valid? report)))
    (is (= #{:multiple-physical-roles}
           (error-kinds report)))
    (is (= #{:browser :server}
           (-> report :errors first :roles)))))

(deftest expected-role-rejects-one-consistent-but-wrong-role
  (let [report
        (preflight/check-plan-registry
         {:plans {:request/claim
                  (one-role-plan :helper)}
          :single-role? true
          :expected-role :request-client})]
    (is (= #{:unexpected-plan-role}
           (error-kinds report)))
    (is (= :request-client
           (-> report :errors first :expected-role)))
    (is (= #{:helper}
           (-> report :errors first :actual-roles)))))

(deftest stale-generated-artifact-digests-fail-preflight
  (let [claim-plan
        (one-role-plan :browser :request/claim)

        current-digest
        (artifact/executable-digest claim-plan)

        mismatch-report
        (preflight/check-plan-registry
         {:plans {:request/claim claim-plan}
          :expected-digests
          {:request/claim (zeros-digest)}})

        missing-report
        (preflight/check-plan-registry
         {:plans {:request/claim claim-plan}
          :expected-digests {}})

        stale-key-report
        (preflight/check-plan-registry
         {:plans {:request/claim claim-plan}
          :expected-digests
          {:request/claim current-digest
           :request/retired current-digest}})]

    (testing "same logical key with changed executable semantics is stale"
      (is (= #{:executable-plan-digest-mismatch}
             (error-kinds mismatch-report)))
      (is (= [{:plan-key :request/claim
               :expected-digest (zeros-digest)
               :actual-digest current-digest}]
             (-> mismatch-report :errors first :mismatches))))

    (testing "a generated digest registry must cover every current plan"
      (is (= #{:missing-expected-digests}
             (error-kinds missing-report)))
      (is (= #{:request/claim}
             (-> missing-report :errors first :missing-keys))))

    (testing "a digest for a retired logical plan is also stale"
      (is (= #{:stale-expected-digests}
             (error-kinds stale-key-report)))
      (is (= #{:request/retired}
             (-> stale-key-report :errors first :stale-keys))))))

(deftest malformed-and-unsupported-plans-are-rejected
  (let [plan
        (one-role-plan :browser)

        malformed
        (dissoc plan :states)

        unsupported
        (assoc plan
               :gesso.choreo/version
               (inc project/executable-plan-version))

        malformed-report
        (preflight/check-plan-registry
         {:plans {:request/claim malformed}})

        unsupported-report
        (preflight/check-plan-registry
         {:plans {:request/claim unsupported}})]

    (is (= #{:invalid-executable-plan}
           (error-kinds malformed-report)))
    (is (= #{:unsupported-executable-plan-version}
           (error-kinds unsupported-report)))))

(deftest invalid-logical-keys-never-enter-the-emitted-registry
  (let [plan
        (one-role-plan :browser)

        report
        (preflight/check-plan-registry
         {:plans {"request/claim" plan
                  :request/cancel plan}
          :required-keys #{:request/claim :request/cancel}})]

    (is (= #{:invalid-plan-key
             :missing-required-plans}
           (error-kinds report)))
    (is (= #{:request/cancel}
           (get-in report [:analysis :plan-keys])))))

(deftest checker-aggregates-independent-application-defects
  (let [plans
        (two-role-plans)

        report
        (preflight/check-plan-registry
         {:plans {:request/claim (:browser plans)
                  :request/debug (:server plans)}
          :required-keys #{:request/claim :request/cancel}
          :single-role? true
          :expected-role :browser
          :expected-digests
          {:request/claim (zeros-digest)}})]

    (is (false? (:valid? report)))
    (is (= #{:missing-required-plans
             :unexpected-plans
             :multiple-physical-roles
             :unexpected-plan-role
             :missing-expected-digests
             :executable-plan-digest-mismatch}
           (error-kinds report)))
    (is (= 6
           (count (:errors report))))))

(deftest empty-registry-is-rejected-unless-explicitly-allowed
  (let [default-report
        (preflight/check-plan-registry
         {:plans {}})

        allowed-report
        (preflight/check-plan-registry
         {:plans {}
          :allow-empty? true
          :single-role? true})

        allowed-registry
        (preflight/require-plan-registry!
         {:name :infrastructure/optional-runtime
          :plans {}
          :allow-empty? true
          :single-role? true})]

    (is (= #{:empty-plan-registry}
           (error-kinds default-report)))
    (is (preflight/valid? allowed-report))
    (is (preflight/plan-registry? allowed-registry))
    (is (= #{} (:roles allowed-registry)))
    (is (= {} (:plans allowed-registry)))
    (is (= {} (:digests allowed-registry)))))

(deftest invalid-checker-options-fail-immediately
  (doseq [[expected-kind options]
          [[:invalid-options nil]
           [:unknown-options {:plans {} :mystery true}]
           [:missing-plans {}]
           [:invalid-name {:plans {} :name "browser"}]
           [:invalid-required-keys {:plans {} :required-keys [:a]}]
           [:invalid-expected-role {:plans {} :expected-role "browser"}]
           [:invalid-boolean-option {:plans {} :single-role? :yes}]
           [:invalid-boolean-option {:plans {} :allow-empty? nil}]
           [:invalid-expected-digests
            {:plans {}
             :expected-digests {:request/claim "ABC"}}]]]
    (let [data
          (error-data
           #(preflight/check-plan-registry options))]
      (is (= :gesso.choreo.preflight/error
             (:error/type data)))
      (is (= expected-kind
             (:error/kind data))))))

(deftest require-plan-registry-preserves-the-complete-failure-report
  (let [data
        (error-data
         #(preflight/require-plan-registry!
           {:plans {}
            :required-keys #{:request/claim}}))

        report
        (:preflight data)]
    (is (= :gesso.choreo.preflight/error
           (:error/type data)))
    (is (= :plan-registry-preflight-failed
           (:error/kind data)))
    (is (preflight/report? report))
    (is (= #{:empty-plan-registry
             :missing-required-plans}
           (error-kinds report)))))

(deftest closed-shape-predicates-reject-tampering
  (let [plan
        (one-role-plan :browser)

        registry
        (preflight/require-plan-registry!
         {:plans {:request/claim plan}})

        report
        (preflight/check-plan-registry
         {:plans {:request/claim plan}})]

    (testing "reports cannot gain undeclared keys or lie about validity"
      (is (false?
           (preflight/report?
            (assoc report :extra true))))
      (is (false?
           (preflight/report?
            (assoc report :valid? false)))))

    (testing "registries cannot gain keys, change plans, roles, or digests"
      (is (false?
           (preflight/plan-registry?
            (assoc registry :extra true))))
      (is (false?
           (preflight/plan-registry?
            (assoc registry :roles #{:server}))))
      (is (false?
           (preflight/plan-registry?
            (assoc-in registry
                      [:digests :request/claim]
                      (zeros-digest))))))))

(deftest explain-rejects-unrecognized-values
  (let [data
        (error-data
         #(preflight/explain {:not :preflight}))]
    (is (= :gesso.choreo.preflight/error
           (:error/type data)))
    (is (= :unsupported-explain-value
           (:error/kind data)))))

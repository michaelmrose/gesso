(ns gesso.live.browser.preflight-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.choreo.core :as choreo]
   [gesso.choreo.preflight :as choreo-preflight]
   [gesso.choreo.project :as project]
   [gesso.live.browser.preflight :as preflight]))

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
     {:name :example/browser-preflight-one-role
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
         {:name :example/browser-preflight-two-role
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

(defn- registry
  ([plans]
   (registry plans {}))
  ([plans options]
   (choreo-preflight/require-plan-registry!
    (merge
     {:plans plans}
     options))))

(deftest successful-browser-preflight-emits-one-closed-assembly-manifest
  (let [plans
        {:request/claim
         (one-role-plan :request-client :request/claim)

         :request/cancel
         (one-role-plan :request-client :request/cancel)}

        plan-registry
        (registry
         plans
         {:name :humanhelp/request-plans
          :required-keys #{:request/claim :request/cancel}
          :single-role? true
          :expected-role :request-client})

        options
        {:name :humanhelp/request-browser
         :plan-registry plan-registry
         :browser-role :request-client
         :required-plan-keys #{:request/claim :request/cancel}
         :optimistic? true
         :optimistic-htmx? true}

        report
        (preflight/check-browser-assembly options)

        manifest
        (preflight/require-browser-assembly! options)]

    (testing "the report closes plan/runtime/transport facts"
      (is (preflight/report? report))
      (is (preflight/valid? report))
      (is (empty? (:errors report)))
      (is (= :request-client
             (get-in report [:analysis :browser-role])))
      (is (= #{:request/claim :request/cancel}
             (get-in report [:analysis :plan-keys])))
      (is (= #{:request-client}
             (get-in report [:analysis :plan-roles])))
      (is (= #{:optimistic :optimistic-htmx}
             (get-in report [:analysis :features])))
      (is (= :htmx
             (get-in report
                     [:analysis :optimistic-command-transport])))
      (is (= preflight/canonical-bootstrap-entrypoint
             (get-in report [:analysis :bootstrap-entrypoint]))))

    (testing "the emitted manifest is the single closed browser-build input"
      (is (preflight/assembly-manifest? manifest))
      (is (= :humanhelp/request-browser
             (:name manifest)))
      (is (= plan-registry
             (:plan-registry manifest)))
      (is (= :request-client
             (:browser-role manifest)))
      (is (= #{:request/claim :request/cancel}
             (:required-plan-keys manifest)))
      (is (= #{:optimistic :optimistic-htmx}
             (:features manifest)))
      (is (= :htmx
             (:optimistic-command-transport manifest)))
      (is (= {:required? true
              :entrypoint preflight/canonical-bootstrap-entrypoint}
             (:bootstrap manifest))))

    (testing "the stable explainer reports the same assembly identity"
      (is (= {:type preflight/assembly-manifest-type
              :version preflight/preflight-version
              :name :humanhelp/request-browser
              :browser-role :request-client
              :plan-count 2
              :plan-keys #{:request/claim :request/cancel}
              :features #{:optimistic :optimistic-htmx}
              :optimistic-command-transport :htmx
              :bootstrap-entrypoint
              preflight/canonical-bootstrap-entrypoint}
             (preflight/explain manifest))))))

(deftest transport-defaults-are-derived-from-the-feature-closure
  (let [plan-registry
        (registry {:request/claim (one-role-plan :browser)})

        base
        {:plan-registry plan-registry
         :browser-role :browser}

        plain
        (preflight/check-browser-assembly base)

        optimistic
        (preflight/check-browser-assembly
         (assoc base :optimistic? true))

        optimistic-htmx
        (preflight/check-browser-assembly
         (assoc base
                :optimistic? true
                :optimistic-htmx? true))]

    (is (= :none
           (get-in plain
                   [:analysis :optimistic-command-transport])))
    (is (= :custom
           (get-in optimistic
                   [:analysis :optimistic-command-transport])))
    (is (= :htmx
           (get-in optimistic-htmx
                   [:analysis :optimistic-command-transport])))
    (is (every? preflight/valid?
                [plain optimistic optimistic-htmx]))))

(deftest one-physical-browser-role-is-enforced
  (let [plans
        (two-role-plans)

        plan-registry
        (registry
         {:browser (:browser plans)
          :server (:server plans)})

        report
        (preflight/check-browser-assembly
         {:plan-registry plan-registry
          :browser-role :browser})]

    (is (false? (preflight/valid? report)))
    (is (= #{:multiple-browser-plan-roles}
           (error-kinds report)))
    (is (= #{:browser :server}
           (-> report :errors first :roles)))))

(deftest one-consistent-but-wrong-browser-role-is-rejected
  (let [plan-registry
        (registry
         {:request/claim
          (one-role-plan :helper)})

        report
        (preflight/check-browser-assembly
         {:plan-registry plan-registry
          :browser-role :request-client})]

    (is (= #{:browser-role-plan-mismatch}
           (error-kinds report)))
    (is (= :request-client
           (-> report :errors first :browser-role)))
    (is (= #{:helper}
           (-> report :errors first :plan-roles)))))

(deftest browser-visible-plan-coverage-is-exact
  (let [plan-registry
        (registry
         {:request/claim
          (one-role-plan :browser :request/claim)
          :request/debug
          (one-role-plan :browser :request/debug)})

        report
        (preflight/check-browser-assembly
         {:plan-registry plan-registry
          :browser-role :browser
          :required-plan-keys
          #{:request/claim :request/cancel}})]

    (is (= #{:missing-browser-plans
             :unexpected-browser-plans}
           (error-kinds report)))
    (is (= #{:request/cancel}
           (->> (:errors report)
                (filter #(= :missing-browser-plans (:kind %)))
                first
                :missing-keys)))
    (is (= #{:request/debug}
           (->> (:errors report)
                (filter #(= :unexpected-browser-plans (:kind %)))
                first
                :unexpected-keys)))))

(deftest an-empty-choreo-registry-cannot-be-installed-into-a-browser-runtime
  (let [plan-registry
        (registry
         {}
         {:allow-empty? true
          :single-role? true})

        report
        (preflight/check-browser-assembly
         {:plan-registry plan-registry
          :browser-role :browser})]

    (is (= #{:empty-browser-plan-registry}
           (error-kinds report)))))

(deftest optimism-and-command-transport-form-a-closed-feature-graph
  (let [plan-registry
        (registry {:request/claim (one-role-plan :browser)})

        base
        {:plan-registry plan-registry
         :browser-role :browser}

        htmx-without-optimism
        (preflight/check-browser-assembly
         (assoc base
                :optimistic? false
                :optimistic-htmx? true
                :optimistic-command-transport :custom))

        optimism-without-transport
        (preflight/check-browser-assembly
         (assoc base
                :optimistic? true
                :optimistic-command-transport :none))

        transport-without-optimism
        (preflight/check-browser-assembly
         (assoc base
                :optimistic? false
                :optimistic-command-transport :custom))

        htmx-transport-without-bridge
        (preflight/check-browser-assembly
         (assoc base
                :optimistic? true
                :optimistic-command-transport :htmx))]

    (testing "HTMX optimism cannot exist without the optimistic runtime or HTMX transport"
      (is (= #{:optimistic-htmx-requires-optimism
               :command-transport-without-optimism
               :optimistic-htmx-requires-htmx-transport}
             (error-kinds htmx-without-optimism))))

    (testing "optimism itself requires some command transport"
      (is (= #{:optimism-requires-command-transport}
             (error-kinds optimism-without-transport))))

    (testing "transport cannot be enabled when optimism is absent"
      (is (= #{:command-transport-without-optimism}
             (error-kinds transport-without-optimism))))

    (testing "the HTMX transport specifically requires the HTMX bridge"
      (is (= #{:htmx-transport-requires-optimistic-htmx}
             (error-kinds htmx-transport-without-bridge))))))

(deftest invalid-choreo-registry-is-rejected-before-browser-assembly
  (let [plan-registry
        (registry {:request/claim (one-role-plan :browser)})

        tampered
        (assoc plan-registry :roles #{:server})

        report
        (preflight/check-browser-assembly
         {:plan-registry tampered
          :browser-role :browser})]

    (is (= #{:invalid-plan-registry}
           (error-kinds report)))))

(deftest browser-preflight-aggregates-independent-assembly-defects
  (let [plans
        (two-role-plans)

        plan-registry
        (registry
         {:request/claim (:browser plans)
          :request/debug (:server plans)})

        report
        (preflight/check-browser-assembly
         {:plan-registry plan-registry
          :browser-role :request-client
          :required-plan-keys
          #{:request/claim :request/cancel}
          :optimistic? false
          :optimistic-htmx? true
          :optimistic-command-transport :custom})]

    (is (false? (preflight/valid? report)))
    (is (= #{:multiple-browser-plan-roles
             :missing-browser-plans
             :unexpected-browser-plans
             :optimistic-htmx-requires-optimism
             :command-transport-without-optimism
             :optimistic-htmx-requires-htmx-transport}
           (error-kinds report)))
    (is (= 6
           (count (:errors report))))))

(deftest invalid-checker-options-fail-immediately
  (let [plan-registry
        (registry {:request/claim (one-role-plan :browser)})]
    (doseq [[expected-kind options]
            [[:invalid-options nil]
             [:unknown-options
              {:plan-registry plan-registry
               :browser-role :browser
               :mystery true}]
             [:missing-plan-registry
              {:browser-role :browser}]
             [:missing-browser-role
              {:plan-registry plan-registry}]
             [:invalid-name
              {:plan-registry plan-registry
               :browser-role :browser
               :name "browser"}]
             [:invalid-browser-role
              {:plan-registry plan-registry
               :browser-role "browser"}]
             [:invalid-required-plan-keys
              {:plan-registry plan-registry
               :browser-role :browser
               :required-plan-keys [:request/claim]}]
             [:invalid-boolean-option
              {:plan-registry plan-registry
               :browser-role :browser
               :optimistic? :yes}]
             [:invalid-boolean-option
              {:plan-registry plan-registry
               :browser-role :browser
               :optimistic-htmx? nil}]
             [:invalid-optimistic-command-transport
              {:plan-registry plan-registry
               :browser-role :browser
               :optimistic-command-transport :websocket}]]]
      (let [data
            (error-data
             #(preflight/check-browser-assembly options))]
        (is (= :gesso.live.browser.preflight/error
               (:error/type data)))
        (is (= expected-kind
               (:error/kind data)))))))

(deftest require-browser-assembly-preserves-the-complete-failure-report
  (let [plan-registry
        (registry {:request/claim (one-role-plan :helper)})

        data
        (error-data
         #(preflight/require-browser-assembly!
           {:plan-registry plan-registry
            :browser-role :request-client
            :required-plan-keys
            #{:request/claim :request/cancel}
            :optimistic? true
            :optimistic-command-transport :none}))

        report
        (:preflight data)]

    (is (= :gesso.live.browser.preflight/error
           (:error/type data)))
    (is (= :browser-assembly-preflight-failed
           (:error/kind data)))
    (is (preflight/report? report))
    (is (= #{:browser-role-plan-mismatch
             :missing-browser-plans
             :optimism-requires-command-transport}
           (error-kinds report)))))

(deftest closed-shape-predicates-reject-report-and-manifest-tampering
  (let [plan-registry
        (registry {:request/claim (one-role-plan :browser)})

        options
        {:name :example/browser
         :plan-registry plan-registry
         :browser-role :browser
         :optimistic? true
         :optimistic-htmx? true}

        report
        (preflight/check-browser-assembly options)

        manifest
        (preflight/require-browser-assembly! options)]

    (testing "reports cannot gain undeclared keys or lie about validity"
      (is (false?
           (preflight/report?
            (assoc report :extra true))))
      (is (false?
           (preflight/report?
            (assoc report :valid? false)))))

    (testing "manifests cannot gain keys or change their role/features/transport"
      (is (false?
           (preflight/assembly-manifest?
            (assoc manifest :extra true))))
      (is (false?
           (preflight/assembly-manifest?
            (assoc manifest :browser-role :server))))
      (is (false?
           (preflight/assembly-manifest?
            (assoc manifest :features #{:optimistic-htmx}))))
      (is (false?
           (preflight/assembly-manifest?
            (assoc manifest
                   :optimistic-command-transport
                   :custom)))))

    (testing "bootstrap is a closed generated contract, not an app-owned boolean"
      (is (false?
           (preflight/assembly-manifest?
            (assoc-in manifest [:bootstrap :required?] false))))
      (is (false?
           (preflight/assembly-manifest?
            (assoc-in manifest
                      [:bootstrap :entrypoint]
                      'example.browser/start!)))))))

(deftest explain-rejects-unrecognized-values
  (let [data
        (error-data
         #(preflight/explain {:not :browser-preflight}))]
    (is (= :gesso.live.browser.preflight/error
           (:error/type data)))
    (is (= :unsupported-explain-value
           (:error/kind data)))))

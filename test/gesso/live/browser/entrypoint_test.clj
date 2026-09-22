(ns gesso.live.browser.entrypoint-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.choreo.artifact :as artifact]
   [gesso.choreo.core :as choreo]
   [gesso.choreo.project :as project]
   [gesso.live.browser.entrypoint :as entrypoint]
   [gesso.live.browser.preflight :as browser-preflight]))

(defn- exception-info-data
  [error]
  (loop [current error]
    (cond
      (nil? current)
      nil

      (instance? clojure.lang.ExceptionInfo current)
      (ex-data current)

      :else
      (recur (.getCause ^Throwable current)))))

(defn- thrown-data
  [f]
  (try
    (f)
    nil
    (catch Throwable error
      (exception-info-data error))))

(defn- preflight-error-kinds
  [data]
  (set
   (map :kind
        (get-in data [:preflight :errors]))))

(defn- one-role-plan
  ([role]
   (one-role-plan role :example/prepare))
  ([role action]
   (project/project
    (choreo/->choreography
     {:name :example/browser-entrypoint-one-role
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
         {:name :example/browser-entrypoint-two-role
          :initial :communicate
          :states
          {:communicate
           (choreo/communicate
            :request-client
            :server
            :example/request
            :done
            {:via :http})

           :done
           (choreo/return :done)}})]
    (project/project-all choreography)))

(def test-browser-role
  :request-client)

(def test-operation-keys
  #{:request/claim
    :request/cancel})

(def test-browser-plans
  {:request/claim
   (one-role-plan test-browser-role :request/claim)

   :request/cancel
   (one-role-plan test-browser-role :request/cancel)})

(def test-plan-digests
  (into {}
        (map
         (fn [[plan-key plan]]
           [plan-key
            (artifact/executable-digest plan)]))
        test-browser-plans))

(def stale-plan-digests
  (assoc
   test-plan-digests
   :request/claim
   (apply str (repeat 64 "0"))))

(def mixed-role-browser-plans
  (let [plans (two-role-plans)]
    {:request/client (:request-client plans)
     :request/server (:server plans)}))

(def wrong-browser-role
  :helper)

(deftest compile-time-var-references-produce-one-closed-browser-manifest
  (let [declaration
        {:name :example/request-browser
         :plans
         'gesso.live.browser.entrypoint-test/test-browser-plans
         :required-plan-keys
         'gesso.live.browser.entrypoint-test/test-operation-keys
         :browser-role
         'gesso.live.browser.entrypoint-test/test-browser-role
         :expected-digests
         'gesso.live.browser.entrypoint-test/test-plan-digests
         :optimistic? true
         :optimistic-htmx? true}

        resolved
        (entrypoint/resolve-declaration! declaration)

        manifest
        (entrypoint/compile-browser-assembly! declaration)]

    (testing "reference-capable declarations resolve only through their JVM Vars"
      (is (= test-browser-plans
             (:plans resolved)))
      (is (= test-operation-keys
             (:required-plan-keys resolved)))
      (is (= test-browser-role
             (:browser-role resolved)))
      (is (= test-plan-digests
             (:expected-digests resolved))))

    (testing "the compiler bridge emits the canonical browser preflight product"
      (is (browser-preflight/assembly-manifest? manifest))
      (is (= :example/request-browser
             (:name manifest)))
      (is (= test-browser-role
             (:browser-role manifest)))
      (is (= test-operation-keys
             (:required-plan-keys manifest)))
      (is (= test-browser-plans
             (get-in manifest [:plan-registry :plans])))
      (is (= test-plan-digests
             (get-in manifest [:plan-registry :digests])))
      (is (= #{:optimistic :optimistic-htmx}
             (:features manifest)))
      (is (= :htmx
             (:optimistic-command-transport manifest)))
      (is (= {:required? true
              :entrypoint
              browser-preflight/canonical-bootstrap-entrypoint}
             (:bootstrap manifest))))

    (testing "the embedded value remains portable closed Clojure data"
      (is (= manifest
             (read-string (pr-str manifest)))))))

(deftest literal-declarations-use-the-same-preflight-path
  (let [manifest
        (entrypoint/compile-browser-assembly!
         {:name :example/literal-browser
          :plans test-browser-plans
          :required-plan-keys test-operation-keys
          :browser-role test-browser-role
          :expected-digests test-plan-digests
          :optimistic? true
          :optimistic-command-transport :custom})]
    (is (browser-preflight/assembly-manifest? manifest))
    (is (= #{:optimistic}
           (:features manifest)))
    (is (= :custom
           (:optimistic-command-transport manifest)))))

(deftest macro-expansion-embeds-the-closed-manifest-without-runtime-reconstruction
  (let [form
        '(gesso.live.browser.entrypoint/embed-browser-assembly
          {:name :example/macro-browser
           :plans
           gesso.live.browser.entrypoint-test/test-browser-plans
           :required-plan-keys
           gesso.live.browser.entrypoint-test/test-operation-keys
           :browser-role
           gesso.live.browser.entrypoint-test/test-browser-role
           :expected-digests
           gesso.live.browser.entrypoint-test/test-plan-digests
           :optimistic? true
           :optimistic-htmx? true})

        expansion
        (macroexpand-1 form)

        embedded
        (second expansion)]

    (is (= 'quote
           (first expansion)))
    (is (= 2
           (count expansion)))
    (is (browser-preflight/assembly-manifest? embedded))
    (is (= :example/macro-browser
           (:name embedded)))
    (is (= test-plan-digests
           (get-in embedded [:plan-registry :digests])))))

(deftest macro-requires-a-literal-declaration-map
  (let [data
        (thrown-data
         #(macroexpand-1
           '(gesso.live.browser.entrypoint/embed-browser-assembly
             declaration-from-runtime)))]
    (is (= :gesso.live.browser.entrypoint/error
           (:error/type data)))
    (is (= :nonliteral-macro-declaration
           (:error/kind data)))))

(deftest compile-time-references-must-be-qualified-resolvable-vars
  (testing "unqualified symbols cannot silently resolve relative to a compiler namespace"
    (let [data
          (thrown-data
           #(entrypoint/resolve-declaration!
             {:plans 'test-browser-plans
              :browser-role test-browser-role}))]
      (is (= :unqualified-compile-time-reference
             (:error/kind data)))
      (is (= :plans
             (:declaration-key data)))))

  (testing "a fully-qualified but nonexistent Var fails before Choreo preflight"
    (let [data
          (thrown-data
           #(entrypoint/resolve-declaration!
             {:plans
              'gesso.live.browser.entrypoint-test/does-not-exist
              :browser-role test-browser-role}))]
      (is (= :compile-time-reference-not-var
             (:error/kind data)))
      (is (= :plans
             (:declaration-key data)))))

  (testing "symbols are forbidden in fields that are not compile-time references"
    (let [data
          (thrown-data
           #(entrypoint/resolve-declaration!
             {:name 'example/name-var
              :plans test-browser-plans
              :browser-role test-browser-role}))]
      (is (= :symbol-not-allowed
             (:error/kind data)))
      (is (= :name
             (:declaration-key data))))))

(deftest arbitrary-declaration-forms-are-data-not-code
  (let [executed?
        (atom false)

        data
        (thrown-data
         #(macroexpand-1
           `(gesso.live.browser.entrypoint/embed-browser-assembly
             {:plans
              (do
                (reset! ~executed? true)
                ~test-browser-plans)
              :browser-role ~test-browser-role})))]
    (is (false? @executed?))
    (is (= :gesso.choreo.preflight/error
           (:error/type data)))
    (is (= :plan-registry-preflight-failed
           (:error/kind data)))
    (is (contains?
         (preflight-error-kinds data)
         :invalid-plan-registry))))

(deftest declaration-shape-errors-fail-before-plan-compilation
  (testing "ordinary callers cannot pass a non-map declaration"
    (let [data
          (thrown-data
           #(entrypoint/resolve-declaration! [:not :a :map]))]
      (is (= :invalid-declaration
             (:error/kind data)))))

  (testing "unknown fields cannot create a parallel application contract"
    (let [data
          (thrown-data
           #(entrypoint/resolve-declaration!
             {:plans test-browser-plans
              :browser-role test-browser-role
              :application-owned-bootstrap true}))]
      (is (= :unknown-declaration-keys
             (:error/kind data)))
      (is (= #{:application-owned-bootstrap}
             (:unknown-keys data)))))

  (testing "plans and browser role are mandatory assembly facts"
    (doseq [[missing-key declaration]
            [[:plans
              {:browser-role test-browser-role}]
             [:browser-role
              {:plans test-browser-plans}]]]
      (let [data
            (thrown-data
             #(entrypoint/resolve-declaration! declaration))]
        (is (= :missing-declaration-key
               (:error/kind data)))
        (is (= missing-key
               (:missing-key data)))))))

(deftest stale-plan-digests-fail-through-the-choreo-preflight-boundary
  (let [data
        (thrown-data
         #(entrypoint/compile-browser-assembly!
           {:plans
            'gesso.live.browser.entrypoint-test/test-browser-plans
            :required-plan-keys
            'gesso.live.browser.entrypoint-test/test-operation-keys
            :browser-role
            'gesso.live.browser.entrypoint-test/test-browser-role
            :expected-digests
            'gesso.live.browser.entrypoint-test/stale-plan-digests}))]
    (is (= :gesso.choreo.preflight/error
           (:error/type data)))
    (is (= :plan-registry-preflight-failed
           (:error/kind data)))
    (is (= #{:executable-plan-digest-mismatch}
           (preflight-error-kinds data)))))

(deftest mixed-physical-roles-fail-before-browser-emission
  (let [data
        (thrown-data
         #(entrypoint/compile-browser-assembly!
           {:plans
            'gesso.live.browser.entrypoint-test/mixed-role-browser-plans
            :browser-role test-browser-role}))]
    (is (= :gesso.choreo.preflight/error
           (:error/type data)))
    (is (= :plan-registry-preflight-failed
           (:error/kind data)))
    (is (= #{:multiple-physical-roles
             :unexpected-plan-role}
           (preflight-error-kinds data)))))

(deftest one-consistent-but-wrong-browser-role-fails-through-choreo-preflight
  (let [data
        (thrown-data
         #(entrypoint/compile-browser-assembly!
           {:plans test-browser-plans
            :browser-role
            'gesso.live.browser.entrypoint-test/wrong-browser-role}))]
    (is (= :plan-registry-preflight-failed
           (:error/kind data)))
    (is (= #{:unexpected-plan-role}
           (preflight-error-kinds data)))))

(deftest browser-feature-contradictions-fail-after-plan-registry-preflight
  (let [data
        (thrown-data
         #(entrypoint/compile-browser-assembly!
           {:plans test-browser-plans
            :browser-role test-browser-role
            :optimistic? false
            :optimistic-htmx? true
            :optimistic-command-transport :custom}))]
    (is (= :gesso.live.browser.preflight/error
           (:error/type data)))
    (is (= :browser-assembly-preflight-failed
           (:error/kind data)))
    (is (= #{:optimistic-htmx-requires-optimism
             :command-transport-without-optimism
             :optimistic-htmx-requires-htmx-transport}
           (preflight-error-kinds data)))))

(deftest exact-required-plan-coverage-is-preserved-through-the-compiler-bridge
  (let [data
        (thrown-data
         #(entrypoint/compile-browser-assembly!
           {:plans test-browser-plans
            :required-plan-keys
            #{:request/claim :request/complete}
            :browser-role test-browser-role}))]
    (is (= :plan-registry-preflight-failed
           (:error/kind data)))
    (is (= #{:missing-required-plans
             :unexpected-plans}
           (preflight-error-kinds data)))))

(deftest resolved-declaration-metadata-is-closed-and-versioned
  (let [resolved
        (entrypoint/resolve-declaration!
         {:name :example/metadata
          :plans test-browser-plans
          :browser-role test-browser-role})]
    (is (= entrypoint/declaration-type
           (:gesso.live.browser.entrypoint/type resolved)))
    (is (= entrypoint/entrypoint-version
           (:gesso.live.browser.entrypoint/version resolved)))
    (is (= #{:gesso.live.browser.entrypoint/type
             :gesso.live.browser.entrypoint/version
             :name
             :plans
             :browser-role}
           (set (keys resolved))))))

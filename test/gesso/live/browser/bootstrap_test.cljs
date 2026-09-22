(ns gesso.live.browser.bootstrap-test
  "Runtime-consumer tests for one preflighted BrowserAssemblyManifest.

   The browser bootstrap is deliberately the only consumer allowed to translate
   a closed BrowserAssemblyManifest into process-default runtime initialization.
   These tests lock down that ownership boundary so applications cannot drift by
   supplying a second browser role, plan registry, plan resolver, or feature set."
  (:require
   [cljs.test :refer-macros [deftest is testing]]
   [gesso.live.browser.bootstrap :as bootstrap]
   [gesso.live.browser.runtime :as runtime]))

;; =============================================================================
;; Helpers / canonical manifest fixture
;; =============================================================================

(defn- thrown
  [f]
  (try
    (f)
    nil
    (catch :default error
      error)))

(defn- error-data
  [f]
  (some-> (thrown f)
          ex-data))

(defn- error-kind
  [f]
  (:error/kind
   (error-data f)))

(def browser-plan
  {:gesso.choreo/type :gesso.choreo/executable-plan
   :gesso.choreo/version 1
   :role :browser
   :initial 0
   :states
   {0 {:op :return
       :outcome :gesso.choreo/complete}}})

(def claim-plan-key
  :request/claim)

(def manifest
  {:gesso.live.browser/type
   :gesso.live.browser.preflight/assembly-manifest

   :gesso.live.browser/version
   1

   :name
   :example/request-browser

   :plan-registry
   {:gesso.choreo/type
    :gesso.choreo.preflight/plan-registry

    :gesso.choreo/version
    1

    :name
    :example/request-plans

    :roles
    #{:browser}

    :plans
    {claim-plan-key browser-plan}

    :digests
    {claim-plan-key
     "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"}}

   :browser-role
   :browser

   :required-plan-keys
   #{claim-plan-key}

   :features
   #{:optimistic :optimistic-htmx}

   :optimistic-command-transport
   :htmx

   :bootstrap
   {:required? true
    :entrypoint 'gesso.live.browser.runtime/init!}})

(defn- physical-realization
  []
  {:core-options
   {:document (js-obj)
    :htmx (js-obj)}

   :choreo-options
   {}

   :optimistic-options
   {:project-provisional (fn [_] nil)
    :render-provisional (fn [& _] nil)
    :refresh-authority (fn [& _] nil)
    :resolve-target (fn [_] nil)
    :process-element (fn [_] nil)}

   :optimistic-htmx-options
   {}})

(defn- fake-runtime
  []
  (js-obj))

(defn- clear-bootstrap!
  []
  (with-redefs [runtime/shutdown! (fn [] :stopped)]
    (bootstrap/shutdown!)))

(defn- with-runtime-stubs
  [f]
  (let [created-runtime (fake-runtime)
        init-calls (atom [])
        shutdown-calls (atom 0)
        default-runtime* (atom nil)]
    (with-redefs
     [runtime/default-runtime
      (fn [] @default-runtime*)

      runtime/init!
      (fn [options]
        (swap! init-calls conj options)
        (reset! default-runtime* created-runtime)
        created-runtime)

      runtime/shutdown!
      (fn []
        (swap! shutdown-calls inc)
        (reset! default-runtime* nil)
        :stopped)

      runtime/runtime?
      (fn [value]
        (identical? value created-runtime))

      runtime/started?
      (fn [value]
        (identical? value created-runtime))

      runtime/optimistic-runtime
      (fn [value]
        (when (identical? value created-runtime)
          {:browser-role :browser}))

      runtime/optimistic-htmx-runtime
      (fn [value]
        (when (identical? value created-runtime)
          {:bridge true}))]
      (f {:runtime created-runtime
          :default-runtime* default-runtime*
          :init-calls init-calls
          :shutdown-calls shutdown-calls}))))

;; =============================================================================
;; Closed manifest / plan lookup
;; =============================================================================

(deftest closed-manifest-and-plan-lookup-test
  (testing "the emitted portable manifest shape is accepted"
    (is (true? (bootstrap/manifest? manifest)))
    (is (= manifest
           (bootstrap/require-manifest! manifest))))

  (testing "plan lookup resolves only the preflighted logical plan registry"
    (is (= browser-plan
           (bootstrap/plan-for manifest claim-plan-key)))
    (is (= browser-plan
           ((bootstrap/plan-resolver manifest)
            {:plan-key claim-plan-key
             :ignored-physical-field :anything}))))

  (testing "missing or malformed logical plan keys fail closed"
    (is (= :invalid-plan-key
           (error-kind
            #(bootstrap/plan-for manifest "request/claim"))))
    (is (= :unregistered-plan-key
           (error-kind
            #(bootstrap/plan-for manifest :request/cancel))))))

(deftest manifest-tampering-is-rejected-test
  (testing "the bootstrap contract is part of the closed manifest"
    (is (false?
         (bootstrap/manifest?
          (assoc-in manifest
                    [:bootstrap :entrypoint]
                    'example.browser/start!))))
    (is (= :invalid-assembly-manifest
           (error-kind
            #(bootstrap/require-manifest!
              (assoc-in manifest
                        [:bootstrap :required?]
                        false))))))

  (testing "registry and physical browser role must remain identical"
    (is (false?
         (bootstrap/manifest?
          (assoc manifest :browser-role :other-browser)))))

  (testing "required plan coverage cannot be weakened after preflight"
    (is (false?
         (bootstrap/manifest?
          (assoc manifest
                 :required-plan-keys
                 #{:request/cancel}))))))

;; =============================================================================
;; Manifest-owned runtime configuration
;; =============================================================================

(deftest runtime-options-derive-manifest-owned-facts-test
  (let [options
        (bootstrap/runtime-options
         manifest
         (physical-realization))

        resolver
        (get-in options
                [:optimistic-htmx-options :plan-for])]
    (testing "the physical browser role is injected by the manifest"
      (is (= :browser
             (get-in options
                     [:optimistic-options :browser-role]))))

    (testing "the HTMX plan resolver comes from the embedded registry"
      (is (fn? resolver))
      (is (= browser-plan
             (resolver {:plan-key claim-plan-key}))))

    (testing "physical realization callbacks remain application supplied"
      (is (fn?
           (get-in options
                   [:optimistic-options :render-provisional])))
      (is (= {}
             (:choreo-options options))))))

(deftest applications-cannot-override-manifest-owned-runtime-facts-test
  (testing "the application cannot select a second browser participant"
    (is (= :manifest-owned-option-override
           (error-kind
            #(bootstrap/runtime-options
              manifest
              (assoc-in
               (physical-realization)
               [:optimistic-options :browser-role]
               :forged-browser))))))

  (testing "the application cannot install a second plan resolver"
    (is (= :manifest-owned-option-override
           (error-kind
            #(bootstrap/runtime-options
              manifest
              (assoc-in
               (physical-realization)
               [:optimistic-htmx-options :plan-for]
               (fn [_] nil))))))))

(deftest realization-options-must-match-manifest-features-test
  (let [non-optimistic
        (-> manifest
            (assoc :features #{})
            (assoc :optimistic-command-transport :none))

        optimistic-without-htmx
        (-> manifest
            (assoc :features #{:optimistic})
            (assoc :optimistic-command-transport :custom))]
    (testing "optimism cannot be physically configured when absent from manifest"
      (is (= :optimistic-options-without-feature
             (error-kind
              #(bootstrap/runtime-options
                non-optimistic
                {:optimistic-options {}})))))

    (testing "enabled optimism requires physical realization callbacks/options"
      (is (= :missing-optimistic-realization
             (error-kind
              #(bootstrap/runtime-options
                optimistic-without-htmx
                {})))))

    (testing "HTMX realization cannot appear when the bridge is absent"
      (is (= :optimistic-htmx-options-without-feature
             (error-kind
              #(bootstrap/runtime-options
                optimistic-without-htmx
                {:optimistic-options {}
                 :optimistic-htmx-options {}})))))))

;; =============================================================================
;; Process-default ownership
;; =============================================================================

(deftest unmanaged-default-runtime-is-rejected-test
  (clear-bootstrap!)
  (let [foreign-runtime (fake-runtime)]
    (with-redefs [runtime/default-runtime (fn [] foreign-runtime)]
      (is (= :unmanaged-default-runtime
             (error-kind
              #(bootstrap/init!
                manifest
                (physical-realization)))))))
  (clear-bootstrap!))

(deftest same-assembly-initialization-is-idempotent-test
  (clear-bootstrap!)
  (with-runtime-stubs
    (fn [{:keys [runtime init-calls]}]
      (let [first-runtime
            (bootstrap/init!
             manifest
             (physical-realization))

            second-runtime
            (bootstrap/init!
             manifest
             (physical-realization))]
        (is (identical? runtime first-runtime))
        (is (identical? first-runtime second-runtime))
        (is (= 1 (count @init-calls)))
        (is (= manifest
               (bootstrap/active-manifest))))))
  (clear-bootstrap!))

(deftest different-assembly-cannot-replace-owned-runtime-test
  (clear-bootstrap!)
  (with-runtime-stubs
    (fn [_]
      (bootstrap/init!
       manifest
       (physical-realization))
      (is (= :different-assembly-already-owned
             (error-kind
              #(bootstrap/init!
                (assoc manifest :name :other/browser)
                (physical-realization)))))))
  (clear-bootstrap!))

(deftest initialized-runtime-must-correspond-to-manifest-test
  (clear-bootstrap!)
  (let [created-runtime (fake-runtime)
        shutdown-calls (atom 0)]
    (with-redefs
     [runtime/default-runtime (fn [] nil)
      runtime/init! (fn [_] created-runtime)
      runtime/runtime? (fn [value] (identical? value created-runtime))
      runtime/started? (fn [value] (identical? value created-runtime))
      runtime/optimistic-runtime (fn [_] nil)
      runtime/optimistic-htmx-runtime (fn [_] {:bridge true})
      runtime/shutdown! (fn [] (swap! shutdown-calls inc) :stopped)]
      (is (= :initialized-runtime-manifest-mismatch
             (error-kind
              #(bootstrap/init!
                manifest
                (physical-realization)))))
      (is (= 1 @shutdown-calls))
      (is (nil? (bootstrap/active-manifest)))))
  (clear-bootstrap!))

(deftest owned-existing-runtime-is-revalidated-before-reuse-test
  (clear-bootstrap!)
  (with-runtime-stubs
    (fn [{:keys [runtime]}]
      (bootstrap/init!
       manifest
       (physical-realization))
      (with-redefs [runtime/optimistic-runtime (fn [_] nil)]
        (is (= :runtime-manifest-mismatch
               (error-kind
                #(bootstrap/init!
                  manifest
                  (physical-realization))))))
      (is (identical? runtime
                      (runtime/default-runtime)))))
  (clear-bootstrap!))

(deftest shutdown-releases-bootstrap-ownership-test
  (clear-bootstrap!)
  (with-runtime-stubs
    (fn [{:keys [shutdown-calls]}]
      (bootstrap/init!
       manifest
       (physical-realization))
      (is (= manifest
             (bootstrap/active-manifest)))
      (is (= :stopped
             (bootstrap/shutdown!)))
      (is (nil? (bootstrap/active-manifest)))
      (is (= 1 @shutdown-calls))))
  (clear-bootstrap!))

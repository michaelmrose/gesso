(ns gesso.live.browser.artifact-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.choreo.core :as choreo]
   [gesso.choreo.preflight :as choreo-preflight]
   [gesso.choreo.project :as project]
   [gesso.live.browser.artifact :as artifact]
   [gesso.live.browser.preflight :as browser-preflight]))

(defn- thrown-data
  [f]
  (try
    (f)
    nil
    (catch clojure.lang.ExceptionInfo ex
      (ex-data ex))))

(defn- error-kinds
  [report]
  (set (map :kind (:errors report))))

(defn- errors-of-kind
  [report kind]
  (filterv #(= kind (:kind %))
           (:errors report)))

(defn- one-role-plan
  [role action]
  (project/project
   (choreo/->choreography
    {:name :example/browser-artifact
     :initial :perform
     :states
     {:perform
      (choreo/local role action :done)

      :done
      (choreo/return :done)}})
   role))

(defn- browser-manifest
  []
  (let [role
        :request-client

        plans
        {:request/claim
         (one-role-plan role :request/claim)

         :request/cancel
         (one-role-plan role :request/cancel)}

        plan-registry
        (choreo-preflight/require-plan-registry!
         {:name :humanhelp/request-plans
          :plans plans
          :required-keys #{:request/claim :request/cancel}
          :single-role? true
          :expected-role role})]
    (browser-preflight/require-browser-assembly!
     {:name :humanhelp/request-browser
      :plan-registry plan-registry
      :browser-role role
      :required-plan-keys #{:request/claim :request/cancel}
      :optimistic? true
      :optimistic-htmx? true})))

(defn- zero-digest
  []
  (apply str (repeat 64 "0")))

(deftest manifest-produces-one-closed-portable-artifact-stamp
  (let [manifest
        (browser-manifest)

        stamp
        (artifact/stamp manifest)]
    (testing "stamping closes the runtime-relevant browser assembly facts"
      (is (artifact/stamp? stamp))
      (is (= artifact/stamp-type
             (:gesso.live.browser.artifact/type stamp)))
      (is (= artifact/artifact-version
             (:gesso.live.browser.artifact/version stamp)))
      (is (= browser-preflight/preflight-version
             (:assembly-manifest-version stamp)))
      (is (= choreo-preflight/preflight-version
             (:plan-registry-version stamp)))
      (is (= :humanhelp/request-browser
             (:assembly-name stamp)))
      (is (= :humanhelp/request-plans
             (:plan-registry-name stamp)))
      (is (= :request-client
             (:browser-role stamp)))
      (is (= #{:request/claim :request/cancel}
             (:required-plan-keys stamp)))
      (is (= (get-in manifest [:plan-registry :digests])
             (:plan-digests stamp)))
      (is (= #{:optimistic :optimistic-htmx}
             (:features stamp)))
      (is (= :htmx
             (:optimistic-command-transport stamp)))
      (is (= browser-preflight/canonical-bootstrap-entrypoint
             (:bootstrap-entrypoint stamp))))

    (testing "the stamp remains closed portable Clojure data"
      (is (= stamp
             (read-string (pr-str stamp))))
      (is (= #{:request/claim :request/cancel}
             (:plan-keys (artifact/explain stamp)))))))

(deftest exact-generated-artifact-correspondence-is-recognized-and-required
  (let [manifest
        (browser-manifest)

        generated-stamp
        (artifact/stamp manifest)

        report
        (artifact/check-correspondence
         manifest
         generated-stamp)]
    (is (artifact/report? report))
    (is (artifact/valid? report))
    (is (empty? (:errors report)))
    (is (empty? (:warnings report)))
    (is (= generated-stamp
           (get-in report [:analysis :current-stamp])))
    (is (= generated-stamp
           (get-in report [:analysis :generated-stamp])))
    (is (= generated-stamp
           (artifact/require-correspondence!
            manifest
            generated-stamp)))
    (is (= {:type artifact/correspondence-report-type
            :version artifact/artifact-version
            :valid? true
            :error-count 0
            :warning-count 0
            :assembly-name :humanhelp/request-browser}
           (artifact/explain report)))))

(deftest executable-plan-drift-fails-closed-with-plan-specific-diagnostics
  (let [manifest
        (browser-manifest)

        current
        (artifact/stamp manifest)

        digest-mismatch
        (assoc-in current
                  [:plan-digests :request/claim]
                  (zero-digest))

        missing-plan
        (-> current
            (update :plan-digests dissoc :request/cancel)
            (update :required-plan-keys disj :request/cancel))

        stale-plan
        (-> current
            (assoc-in [:plan-digests :request/reassign]
                      (zero-digest))
            (update :required-plan-keys conj :request/reassign))

        mismatch-report
        (artifact/check-correspondence manifest digest-mismatch)

        missing-report
        (artifact/check-correspondence manifest missing-plan)

        stale-report
        (artifact/check-correspondence manifest stale-plan)]
    (testing "different executable content is rejected even when plan identity is unchanged"
      (is (artifact/stamp? digest-mismatch))
      (is (contains?
           (error-kinds mismatch-report)
           :generated-artifact-plan-digest-mismatch))
      (is (= [{:plan-key :request/claim
               :current-digest
               (get-in current [:plan-digests :request/claim])
               :generated-digest (zero-digest)}]
             (:mismatches
              (first
               (errors-of-kind
                mismatch-report
                :generated-artifact-plan-digest-mismatch)))))
      (is (false? (artifact/valid? mismatch-report))))

    (testing "missing generated plans are distinguished from changed plan content"
      (is (artifact/stamp? missing-plan))
      (is (contains?
           (error-kinds missing-report)
           :generated-artifact-missing-plan-digests))
      (is (= #{:request/cancel}
             (:missing-plan-keys
              (first
               (errors-of-kind
                missing-report
                :generated-artifact-missing-plan-digests)))))
      (is (contains?
           (error-kinds missing-report)
           :browser-artifact-field-mismatch)))

    (testing "generated plans no longer present in the current assembly are stale"
      (is (artifact/stamp? stale-plan))
      (is (contains?
           (error-kinds stale-report)
           :generated-artifact-stale-plan-digests))
      (is (= #{:request/reassign}
             (:stale-plan-keys
              (first
               (errors-of-kind
                stale-report
                :generated-artifact-stale-plan-digests)))))
      (is (contains?
           (error-kinds stale-report)
           :browser-artifact-field-mismatch)))))

(deftest runtime-assembly-drift-fails-even-when-plan-digests-still-match
  (let [manifest
        (browser-manifest)

        current
        (artifact/stamp manifest)

        wrong-assembly-name
        (assoc current
               :assembly-name
               :humanhelp/stale-request-browser)

        wrong-registry-name
        (assoc current
               :plan-registry-name
               :humanhelp/stale-request-plans)

        wrong-role
        (assoc current
               :browser-role
               :helper)

        wrong-feature-closure
        (assoc current
               :features #{:optimistic}
               :optimistic-command-transport :custom)]
    (doseq [[field generated-stamp]
            [[:assembly-name wrong-assembly-name]
             [:plan-registry-name wrong-registry-name]
             [:browser-role wrong-role]]]
      (testing (str "valid generated stamp with stale " field " is rejected")
        (is (artifact/stamp? generated-stamp))
        (let [report
              (artifact/check-correspondence
               manifest
               generated-stamp)

              mismatches
              (errors-of-kind
               report
               :browser-artifact-field-mismatch)]
          (is (false? (artifact/valid? report)))
          (is (some #(= field (:field %))
                    mismatches)))))

    (testing "feature closure and transport move together but must still match the current build"
      (is (artifact/stamp? wrong-feature-closure))
      (let [report
            (artifact/check-correspondence
             manifest
             wrong-feature-closure)

            fields
            (set
             (map :field
                  (errors-of-kind
                   report
                   :browser-artifact-field-mismatch)))]
        (is (false? (artifact/valid? report)))
        (is (= #{:features
                 :optimistic-command-transport}
               fields))))))

(deftest malformed-or-incoherent-generated-stamps-never-become-correspondence
  (let [manifest
        (browser-manifest)

        current
        (artifact/stamp manifest)

        malformed-values
        [(dissoc current :browser-role)
         (assoc current :unexpected true)
         (assoc-in current
                   [:plan-digests :request/claim]
                   "not-a-sha-256")
         (assoc current
                :features #{:optimistic}
                :optimistic-command-transport :none)
         (assoc current
                :gesso.live.browser.artifact/version
                (inc artifact/artifact-version))
         (assoc current
                :bootstrap-entrypoint
                'example.browser/wrong-start!)]]
    (doseq [generated-stamp malformed-values]
      (is (false? (artifact/stamp? generated-stamp)))
      (let [report
            (artifact/check-correspondence
             manifest
             generated-stamp)]
        (is (artifact/report? report))
        (is (false? (artifact/valid? report)))
        (is (= #{:invalid-generated-artifact-stamp}
               (error-kinds report)))))))

(deftest forged-positive-correspondence-report-is-not-recognized
  (let [manifest
        (browser-manifest)

        current
        (artifact/stamp manifest)

        genuine-report
        (artifact/check-correspondence manifest current)

        different-valid-stamp
        (assoc current :browser-role :helper)

        forged-report
        (assoc genuine-report
               :valid? true
               :errors []
               :analysis
               {:current-stamp current
                :generated-stamp different-valid-stamp})]
    (is (artifact/stamp? different-valid-stamp))
    (is (artifact/report? genuine-report))
    (is (artifact/valid? genuine-report))

    (testing "success cannot be asserted independently of exact stamp equality"
      (is (false? (artifact/report? forged-report)))
      (is (false? (artifact/valid? forged-report))))))

(deftest require-correspondence-preserves-the-complete-failure-report
  (let [manifest
        (browser-manifest)

        current
        (artifact/stamp manifest)

        stale
        (assoc-in current
                  [:plan-digests :request/claim]
                  (zero-digest))

        data
        (thrown-data
         #(artifact/require-correspondence!
           manifest
           stale))]
    (is (= :gesso.live.browser.artifact/error
           (:error/type data)))
    (is (= :browser-artifact-correspondence-failed
           (:error/kind data)))
    (is (artifact/report?
         (:correspondence data)))
    (is (= #{:generated-artifact-plan-digest-mismatch}
           (error-kinds (:correspondence data))))))

(deftest invalid-current-manifests-cannot-be-stamped-or-used-as-correspondence-truth
  (let [manifest
        (browser-manifest)

        generated-stamp
        (artifact/stamp manifest)

        tampered-manifest
        (assoc manifest :browser-role :helper)

        stamp-error
        (thrown-data
         #(artifact/stamp tampered-manifest))

        correspondence-error
        (thrown-data
         #(artifact/check-correspondence
           tampered-manifest
           generated-stamp))]
    (is (= :invalid-browser-assembly-manifest
           (:error/kind stamp-error)))
    (is (= :invalid-current-browser-assembly-manifest
           (:error/kind correspondence-error)))))

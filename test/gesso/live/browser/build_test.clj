(ns gesso.live.browser.build-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.choreo.core :as choreo]
   [gesso.choreo.preflight :as choreo-preflight]
   [gesso.choreo.project :as project]
   [gesso.live.browser.artifact :as artifact]
   [gesso.live.browser.build :as build]
   [gesso.live.browser.preflight :as browser-preflight])
  (:import
   (java.nio.file Files)))

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

(defn- delete-tree!
  [root]
  (doseq [file
          (reverse
           (file-seq root))]
    (Files/deleteIfExists
     (.toPath file))))

(defn- with-temp-dir
  [f]
  (let [path
        (Files/createTempDirectory
         "gesso-browser-build-test-"
         (make-array java.nio.file.attribute.FileAttribute 0))

        dir
        (.toFile path)]
    (try
      (f dir)
      (finally
        (delete-tree! dir)))))

(defn- child-path
  [dir name]
  (str
   (.resolve
    (.toPath dir)
    name)))

(defn- write-js!
  [path content]
  (spit path content :encoding "UTF-8")
  path)

(defn- write-receipt-value!
  [path value]
  (spit path
        (str (pr-str value) "\n")
        :encoding "UTF-8")
  value)

(defn- one-role-plan
  [role action]
  (project/project
   (choreo/->choreography
    {:name :example/browser-build
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

(defn- exact-js
  []
  "console.log('gesso browser artifact');\n")

(deftest generated-artifact-descriptor-is-content-addressed-and-nonempty
  (with-temp-dir
    (fn [dir]
      (let [artifact-path
            (child-path dir "gesso-live.js")]
        (write-js! artifact-path (exact-js))
        (let [descriptor
              (build/artifact-descriptor artifact-path)]
          (is (= :sha-256
                 (:algorithm descriptor)))
          (is (boolean
               (re-matches #"[0-9a-f]{64}"
                           (:digest descriptor))))
          (is (= (count
                  (.getBytes
                   (exact-js)
                   java.nio.charset.StandardCharsets/UTF_8))
                 (:size descriptor))))

        (testing "missing and empty artifacts fail before receipt construction"
          (is (= :missing-generated-artifact
                 (:error/kind
                  (thrown-data
                   #(build/artifact-descriptor
                     (child-path dir "missing.js"))))))

          (let [empty-path
                (child-path dir "empty.js")]
            (write-js! empty-path "")
            (is (= :empty-generated-artifact
                   (:error/kind
                    (thrown-data
                     #(build/artifact-descriptor empty-path)))))))))))

(deftest exact-record-read-and-verify-round-trip-is-closed
  (with-temp-dir
    (fn [dir]
      (let [manifest
            (browser-manifest)

            artifact-path
            (child-path dir "gesso-live.js")

            receipt-path
            (build/receipt-path artifact-path)]
        (write-js! artifact-path (exact-js))
        (let [recorded
              (build/record-generated-artifact!
               manifest
               artifact-path)

              read-back
              (build/read-artifact-receipt!
               receipt-path)

              verified
              (build/verify-generated-artifact!
               manifest
               artifact-path)]
          (is (build/receipt? recorded))
          (is (= recorded read-back))
          (is (= recorded verified))
          (is (= (artifact/stamp manifest)
                 (:stamp recorded)))
          (is (= (build/artifact-descriptor artifact-path)
                 (:artifact recorded)))
          (is (.isFile
               (java.io.File. receipt-path))))))))

(deftest custom-receipt-path-is-supported-without-expanding-option-vocabulary
  (with-temp-dir
    (fn [dir]
      (let [manifest
            (browser-manifest)

            artifact-path
            (child-path dir "gesso-live.js")

            metadata-path
            (child-path dir "metadata/browser.edn")]
        (write-js! artifact-path (exact-js))
        (let [receipt
              (build/record-generated-artifact!
               manifest
               artifact-path
               {:receipt-path metadata-path})]
          (is (= receipt
                 (build/verify-generated-artifact!
                  manifest
                  artifact-path
                  {:receipt-path metadata-path}))))

        (is (= :unknown-record-options
               (:error/kind
                (thrown-data
                 #(build/record-generated-artifact!
                   manifest
                   artifact-path
                   {:unknown true})))))

        (is (= :unknown-verify-options
               (:error/kind
                (thrown-data
                 #(build/verify-generated-artifact!
                   manifest
                   artifact-path
                   {:unknown true})))))))))

(deftest missing-unreadable-and-multiform-receipts-fail-closed
  (with-temp-dir
    (fn [dir]
      (let [manifest
            (browser-manifest)

            artifact-path
            (child-path dir "gesso-live.js")

            receipt-path
            (build/receipt-path artifact-path)]
        (write-js! artifact-path (exact-js))

        (testing "missing receipt is distinct from missing artifact"
          (is (= :missing-artifact-receipt
                 (:error/kind
                  (thrown-data
                   #(build/verify-generated-artifact!
                     manifest
                     artifact-path))))))

        (testing "non-EDN receipt is unreadable"
          (spit receipt-path "{not valid edn" :encoding "UTF-8")
          (is (= :unreadable-artifact-receipt
                 (:error/kind
                  (thrown-data
                   #(build/read-artifact-receipt!
                     receipt-path))))))

        (testing "exactly one EDN form is required"
          (let [receipt
                (build/artifact-receipt
                 manifest
                 artifact-path)]
            (spit receipt-path
                  (str (pr-str receipt)
                       "\n{:unexpected :second-form}\n")
                  :encoding "UTF-8")
            (is (= :unreadable-artifact-receipt
                   (:error/kind
                    (thrown-data
                     #(build/read-artifact-receipt!
                       receipt-path)))))))

        (testing "trailing whitespace and comments do not create a second form"
          (let [receipt
                (build/artifact-receipt
                 manifest
                 artifact-path)]
            (spit receipt-path
                  (str (pr-str receipt)
                       "\n\n; build comment\n  \t\n")
                  :encoding "UTF-8")
            (is (= receipt
                   (build/read-artifact-receipt!
                    receipt-path)))))))))

(deftest malformed-or-unsupported-receipt-shapes-are-rejected-before-correspondence
  (with-temp-dir
    (fn [dir]
      (let [manifest
            (browser-manifest)

            artifact-path
            (child-path dir "gesso-live.js")

            receipt-path
            (build/receipt-path artifact-path)]
        (write-js! artifact-path (exact-js))
        (let [receipt
              (build/artifact-receipt
               manifest
               artifact-path)

              malformed-values
              [(assoc receipt
                      :gesso.live.browser.build/version
                      (inc build/build-version))
               (assoc receipt :unexpected true)
               (assoc-in receipt [:artifact :algorithm] :sha-1)
               (assoc-in receipt [:artifact :digest] "not-a-digest")
               (assoc-in receipt [:artifact :size] 0)
               (update receipt :stamp dissoc :browser-role)]]
          (doseq [value malformed-values]
            (is (false? (build/receipt? value)))
            (write-receipt-value! receipt-path value)
            (let [data
                  (thrown-data
                   #(build/read-artifact-receipt!
                     receipt-path))]
              (is (= :invalid-artifact-receipt
                     (:error/kind data)))
              (is (= value
                     (:receipt data))))))))))

(deftest stale-semantic-stamp-is-distinct-from-physical-byte-drift
  (with-temp-dir
    (fn [dir]
      (let [manifest
            (browser-manifest)

            stale-manifest
            (assoc manifest
                   :name
                   :humanhelp/stale-request-browser)

            artifact-path
            (child-path dir "gesso-live.js")

            receipt-path
            (build/receipt-path artifact-path)]
        (write-js! artifact-path (exact-js))
        (is (browser-preflight/assembly-manifest?
             stale-manifest))

        (write-receipt-value!
         receipt-path
         (build/artifact-receipt
          stale-manifest
          artifact-path))

        (let [data
              (thrown-data
               #(build/verify-generated-artifact!
                 manifest
                 artifact-path))]
          (is (= :gesso.live.browser.build/error
                 (:error/type data)))
          (is (= :artifact-stamp-correspondence-failed
                 (:error/kind data)))
          (is (= :humanhelp/request-browser
                 (get-in data
                         [:correspondence
                          :analysis
                          :current-stamp
                          :assembly-name])))
          (is (= :humanhelp/stale-request-browser
                 (get-in data
                         [:correspondence
                          :analysis
                          :generated-stamp
                          :assembly-name]))))))))

(deftest changed-truncated-and-empty-artifact-bytes-fail-before-consumption
  (with-temp-dir
    (fn [dir]
      (let [manifest
            (browser-manifest)

            artifact-path
            (child-path dir "gesso-live.js")]
        (write-js! artifact-path (exact-js))
        (build/record-generated-artifact!
         manifest
         artifact-path)

        (testing "same-size changed bytes are detected by digest"
          (let [original
                (exact-js)

                changed
                (str (subs original 0 (dec (count original)))
                     "X")]
            (is (= (count original)
                   (count changed)))
            (write-js! artifact-path changed)
            (let [data
                  (thrown-data
                   #(build/verify-generated-artifact!
                     manifest
                     artifact-path))]
              (is (= :generated-artifact-content-mismatch
                     (:error/kind data)))
              (is (= (get-in data [:recorded :size])
                     (get-in data [:current :size])))
              (is (not=
                   (get-in data [:recorded :digest])
                   (get-in data [:current :digest]))))))

        (testing "truncation is detected by descriptor mismatch"
          (write-js! artifact-path "console.log('truncated');\n")
          (let [data
                (thrown-data
                 #(build/verify-generated-artifact!
                   manifest
                   artifact-path))]
            (is (= :generated-artifact-content-mismatch
                   (:error/kind data)))
            (is (not=
                 (get-in data [:recorded :size])
                 (get-in data [:current :size])))))

        (testing "empty replacement fails at the physical artifact boundary"
          (write-js! artifact-path "")
          (is (= :empty-generated-artifact
                 (:error/kind
                  (thrown-data
                   #(build/verify-generated-artifact!
                     manifest
                     artifact-path))))))))))

(deftest invalid-current-manifest-is-not-misdiagnosed-as-stale-generated-artifact
  (with-temp-dir
    (fn [dir]
      (let [manifest
            (browser-manifest)

            artifact-path
            (child-path dir "gesso-live.js")

            invalid-current
            (assoc manifest :browser-role :helper)]
        (write-js! artifact-path (exact-js))
        (build/record-generated-artifact!
         manifest
         artifact-path)

        (let [data
              (thrown-data
               #(build/verify-generated-artifact!
                 invalid-current
                 artifact-path))]
          (is (= :gesso.live.browser.artifact/error
                 (:error/type data)))
          (is (= :invalid-current-browser-assembly-manifest
                 (:error/kind data))))))))

(deftest receipt-recognition-does-not-claim-semantic-or-physical-correspondence
  (with-temp-dir
    (fn [dir]
      (let [manifest
            (browser-manifest)

            stale-manifest
            (assoc manifest
                   :name
                   :humanhelp/stale-request-browser)

            artifact-path
            (child-path dir "gesso-live.js")]
        (write-js! artifact-path (exact-js))
        (let [stale-receipt
              (build/artifact-receipt
               stale-manifest
               artifact-path)]
          (is (build/receipt? stale-receipt))
          (is (artifact/stamp?
               (:stamp stale-receipt)))
          (is (not=
               (artifact/stamp manifest)
               (:stamp stale-receipt))))))))

(deftest supported-build-publishes-one-verified-staged-artifact
  (with-temp-dir
    (fn [dir]
      (let [manifest
            (browser-manifest)

            artifact-path
            (child-path dir "gesso-live.js")

            staged-path
            (atom nil)

            receipt
            (build/build-generated-artifact!
             manifest
             artifact-path
             (fn [output-path]
               (reset! staged-path output-path)
               (is (not= artifact-path output-path))
               (is (.endsWith output-path "gesso-live.js"))
               (write-js! output-path (exact-js))))]
        (is (build/receipt? receipt))
        (is (= receipt
               (build/verify-generated-artifact!
                manifest
                artifact-path)))
        (is (= (exact-js)
               (slurp artifact-path :encoding "UTF-8")))
        (is (.isFile
             (java.io.File.
              (build/receipt-path artifact-path))))
        (is (some? @staged-path))
        (is (false?
             (.exists
              (java.io.File. @staged-path))))))))

(deftest supported-build-rejects-invalid-input-before-emission
  (with-temp-dir
    (fn [dir]
      (let [manifest
            (browser-manifest)

            artifact-path
            (child-path dir "gesso-live.js")

            calls
            (atom 0)

            emitter
            (fn [output-path]
              (swap! calls inc)
              (write-js! output-path (exact-js)))]
        (testing "invalid current assembly is rejected before the emitter is called"
          (let [data
                (thrown-data
                 #(build/build-generated-artifact!
                   (assoc manifest :browser-role :helper)
                   artifact-path
                   emitter))]
            (is (= :gesso.live.browser.artifact/error
                   (:error/type data)))
            (is (= :invalid-browser-assembly-manifest
                   (:error/kind data)))
            (is (zero? @calls))
            (is (false? (.exists (java.io.File. artifact-path))))))

        (testing "emitter and option contracts fail before touching output"
          (is (= :invalid-artifact-emitter
                 (:error/kind
                  (thrown-data
                   #(build/build-generated-artifact!
                     manifest
                     artifact-path
                     nil)))))
          (is (= :unknown-build-options
                 (:error/kind
                  (thrown-data
                   #(build/build-generated-artifact!
                     manifest
                     artifact-path
                     emitter
                     {:unknown true})))))
          (is (zero? @calls))
          (is (false? (.exists (java.io.File. artifact-path)))))))))

(deftest emission-failure-preserves-the-last-verified-artifact-pair
  (with-temp-dir
    (fn [dir]
      (let [manifest
            (browser-manifest)

            artifact-path
            (child-path dir "gesso-live.js")

            receipt-path
            (build/receipt-path artifact-path)

            previous-js
            "console.log('previous verified runtime');\n"]
        (write-js! artifact-path previous-js)
        (let [previous-receipt
              (build/record-generated-artifact!
               manifest
               artifact-path)

              data
              (thrown-data
               #(build/build-generated-artifact!
                 manifest
                 artifact-path
                 (fn [output-path]
                   ;; Even a partially emitted staged artifact must never replace
                   ;; the last verified runtime when the compiler fails.
                   (write-js! output-path
                              "console.log('partial');\n")
                   (throw
                    (ex-info "compiler failed"
                             {:phase :compile})))))]
          (is (= :gesso.live.browser.build/error
                 (:error/type data)))
          (is (= :generated-artifact-emission-failed
                 (:error/kind data)))
          (is (= previous-js
                 (slurp artifact-path :encoding "UTF-8")))
          (is (= previous-receipt
                 (build/read-artifact-receipt!
                  receipt-path)))
          (is (= previous-receipt
                 (build/verify-generated-artifact!
                  manifest
                  artifact-path)))
          (is (false?
               (.exists
                (java.io.File.
                 (:staged-path data)))))))))

(deftest staged-output-must-exist-and-be-nonempty-before-publication
  (with-temp-dir
    (fn [dir]
      (let [manifest
            (browser-manifest)

            artifact-path
            (child-path dir "gesso-live.js")

            receipt-path
            (build/receipt-path artifact-path)

            previous-js
            "console.log('previous');\n"]
        (write-js! artifact-path previous-js)
        (let [previous-receipt
              (build/record-generated-artifact!
               manifest
               artifact-path)]
          (testing "an emitter that produces no staged artifact fails closed"
            (let [data
                  (thrown-data
                   #(build/build-generated-artifact!
                     manifest
                     artifact-path
                     (fn [_output-path]
                       :did-not-emit)))]
              (is (= :missing-generated-artifact
                     (:error/kind data)))
              (is (= previous-js
                     (slurp artifact-path :encoding "UTF-8")))
              (is (= previous-receipt
                     (build/read-artifact-receipt!
                      receipt-path)))))

          (testing "an empty staged artifact fails closed"
            (let [data
                  (thrown-data
                   #(build/build-generated-artifact!
                     manifest
                     artifact-path
                     (fn [output-path]
                       (write-js! output-path ""))))]
              (is (= :empty-generated-artifact
                     (:error/kind data)))
              (is (= previous-js
                     (slurp artifact-path :encoding "UTF-8")))
              (is (= previous-receipt
                     (build/verify-generated-artifact!
                      manifest
                      artifact-path)))))

          (testing "writing an unrelated path does not satisfy staged ownership"
            (let [wrong-path
                  (child-path dir "wrong-output.js")

                  data
                  (thrown-data
                   #(build/build-generated-artifact!
                     manifest
                     artifact-path
                     (fn [_output-path]
                       (write-js! wrong-path (exact-js)))))]
              (is (= :missing-generated-artifact
                     (:error/kind data)))
              (is (.isFile
                   (java.io.File. wrong-path)))
              (is (= previous-js
                     (slurp artifact-path :encoding "UTF-8")))
              (is (= previous-receipt
                     (build/verify-generated-artifact!
                      manifest
                      artifact-path))))))))))

(deftest supported-build-honors-one-explicit-receipt-location
  (with-temp-dir
    (fn [dir]
      (let [manifest
            (browser-manifest)

            artifact-path
            (child-path dir "gesso-live.js")

            custom-receipt-path
            (child-path dir "metadata/browser.edn")

            default-receipt-path
            (build/receipt-path artifact-path)

            receipt
            (build/build-generated-artifact!
             manifest
             artifact-path
             (fn [output-path]
               (write-js! output-path (exact-js)))
             {:receipt-path custom-receipt-path})]
        (is (= receipt
               (build/verify-generated-artifact!
                manifest
                artifact-path
                {:receipt-path custom-receipt-path})))
        (is (.isFile
             (java.io.File. custom-receipt-path)))
        (is (false?
             (.exists
              (java.io.File. default-receipt-path))))))))

(deftest stale-receipt-cannot-bless-newly-published-bytes-after-receipt-write-failure
  (with-temp-dir
    (fn [dir]
      (let [manifest
            (browser-manifest)

            artifact-path
            (child-path dir "gesso-live.js")

            default-receipt-path
            (build/receipt-path artifact-path)

            impossible-receipt-path
            (child-path dir "receipt-target")

            previous-js
            "console.log('previous');\n"

            next-js
            "console.log('next runtime');\n"]
        (write-js! artifact-path previous-js)
        (let [previous-receipt
              (build/record-generated-artifact!
               manifest
               artifact-path)]
          ;; A non-empty directory cannot be atomically replaced by the receipt
          ;; file on ordinary filesystems, forcing receipt persistence to fail
          ;; after the staged JavaScript has already been published.
          (.mkdirs (java.io.File. impossible-receipt-path))
          (write-js!
           (child-path (java.io.File. impossible-receipt-path)
                       "keep")
           "not metadata")

          (let [data
                (thrown-data
                 #(build/build-generated-artifact!
                   manifest
                   artifact-path
                   (fn [output-path]
                     (write-js! output-path next-js))
                   {:receipt-path impossible-receipt-path}))]
            (is (= :gesso.live.browser.build/error
                   (:error/type data)))
            (is (= :artifact-receipt-publication-failed
                   (:error/kind data)))
            (is (= {:artifact-path artifact-path
                    :receipt-path impossible-receipt-path}
                   (select-keys data
                                [:artifact-path
                                 :receipt-path])))
            (is (= {:artifact (build/artifact-descriptor artifact-path)
                    :stamp (artifact/stamp manifest)}
                   (select-keys data
                                [:artifact
                                 :stamp]))))

          (is (= next-js
                 (slurp artifact-path :encoding "UTF-8")))
          (is (= previous-receipt
                 (build/read-artifact-receipt!
                  default-receipt-path)))

          (let [data
                (thrown-data
                 #(build/verify-generated-artifact!
                   manifest
                   artifact-path))]
            (is (= :generated-artifact-content-mismatch
                   (:error/kind data)))
            (is (= (:artifact previous-receipt)
                   (:recorded data)))
            (is (not=
                 (get-in data [:recorded :digest])
                 (get-in data [:current :digest]))))))))))

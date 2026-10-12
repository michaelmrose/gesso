(ns gesso.live.application-preflight.lifecycle-integration-test
  "End-to-end integration tests for canonical Biff lifecycle enforcement and
   Gesso/Biff startup.  These tests are copied without changing their bodies
   from gesso.live.application-preflight-test (v758); the original remains
   in place until the v760 migration removal.  Unlike direct adapter tests,
   these exercise real application assembly and, where appropriate, biff/start."
  (:require
   [clojure.test :refer [deftest is]]
   [com.biffweb.core :as biff]
   [gesso.core :as g]
   [gesso.live.application-preflight :as application]
   [gesso.live.application-preflight.fixture
    :refer [error-data error-kind error-kinds html-response
            standard-operations render-context rendered-operation-button
            with-closed-application with-surfaced-application]]))

;; =============================================================================
;; v704 released Biff 2 lifecycle module boundary
;; =============================================================================

(def module-test-modules
  [application/application-handler-module])

(defn lifecycle-var-handler
  [_request]
  (g/html-response [:main [:p "var-handler"]]))

(defn- valid-lifecycle-handler
  [ctx]
  (fn [_request]
    (g/html-response
     [:main
      (rendered-operation-button
       ctx
       :request/claim
       "/operations/request/claim")])))

(defn- invalid-lifecycle-handler
  [ctx]
  (fn [_request]
    (g/html-response
     [:main
      (rendered-operation-button
       ctx
       :request/claim
       "/operations/request/not-claim")])))

(defn- application-handler-start
  []
  (:biff.core/start application/application-handler-module))

(defn- application-handler-system
  [assembly handler & kvs]
  (merge
   {application/application-assembly-system-key assembly
    :biff.ring/handler handler}
   (apply hash-map kvs)))

(deftest application-handler-module-has-stable-native-biff-identity
  (is (= application/application-handler-module-id
         (:biff.core/id application/application-handler-module)))
  (is (qualified-keyword? application/application-handler-module-id))
  (is (fn? (:biff.core/start application/application-handler-module)))
  (is (nil? (:biff.core/stop application/application-handler-module))))

(deftest application-handler-module-integrates-through-real-biff-start
  (let [operations (standard-operations)
        ctx (render-context operations)]
    (with-surfaced-application
      {:request-board
       [:main
        (rendered-operation-button
         ctx :request/claim "/operations/request/claim")]}
      (fn [{:keys [assembly]}]
        (let [original-handler (valid-lifecycle-handler ctx)
              observed-handler (atom nil)
              observed-assembly-key? (atom nil)
              observer-id :fixture/observe-gesso-handler
              observer-module
              {:biff.core/id observer-id
               :biff.core/start
               (fn [system]
                 (reset! observed-handler (:biff.ring/handler system))
                 (reset! observed-assembly-key?
                         (contains?
                          system
                          application/application-assembly-system-key))
                 (assoc system :fixture/observer-saw-handler? true))}
              initial-system
              (application-handler-system
               assembly
               original-handler
               :fixture/preserved {:sentinel 42})]
          (with-redefs [module-test-modules
                        [application/application-handler-module
                         observer-module]]
            (let [started
                  (biff/start
                   initial-system
                   #'module-test-modules
                   [application/application-handler-module-id
                    observer-id])]
              (is (= {:sentinel 42} (:fixture/preserved started)))
              (is (true? (:fixture/observer-saw-handler? started)))
              (is (false? @observed-assembly-key?))
              (is (not (contains?
                        started
                        application/application-assembly-system-key)))
              (is (fn? @observed-handler))
              (is (not (identical? original-handler @observed-handler)))
              (is (identical? @observed-handler (:biff.ring/handler started)))
              (is (= 200
                     (:status
                      ((:biff.ring/handler started) {:uri "/"})))))))))))

(deftest application-handler-module-preserves-system-except-handler-and-temporary-assembly
  (let [operations (standard-operations)
        ctx (render-context operations)]
    (with-surfaced-application
      {:request-board
       [:main
        (rendered-operation-button
         ctx :request/claim "/operations/request/claim")]}
      (fn [{:keys [assembly]}]
        (let [handler (valid-lifecycle-handler ctx)
              system
              (application-handler-system
               assembly
               handler
               :fixture/a 1
               :fixture/b {:nested true})
              updated ((application-handler-start) system)]
          (is (= 1 (:fixture/a updated)))
          (is (= {:nested true} (:fixture/b updated)))
          (is (= (-> system
                     (dissoc :biff.ring/handler)
                     (dissoc application/application-assembly-system-key))
                 (dissoc updated :biff.ring/handler)))
          (is (not (contains?
                    updated
                    application/application-assembly-system-key)))
          (is (fn? (:biff.ring/handler updated)))
          (is (not (identical? handler (:biff.ring/handler updated)))))))))

(deftest application-handler-module-accepts-var-handler
  (let [operations (standard-operations)
        ctx (render-context operations)]
    (with-surfaced-application
      {:request-board
       [:main
        (rendered-operation-button
         ctx :request/claim "/operations/request/claim")]}
      (fn [{:keys [assembly]}]
        (let [updated
              ((application-handler-start)
               (application-handler-system
                assembly
                #'lifecycle-var-handler))
              response ((:biff.ring/handler updated) {:uri "/"})]
          (is (fn? (:biff.ring/handler updated)))
          (is (= 200 (:status response)))
          (is (string? (:body response))))))))

(deftest application-handler-module-rejects-invalid-system-assembly-and-handler
  (let [operations (standard-operations)
        ctx (render-context operations)]
    (with-surfaced-application
      {:request-board
       [:main
        (rendered-operation-button
         ctx :request/claim "/operations/request/claim")]}
      (fn [{:keys [assembly]}]
        (let [start (application-handler-start)]
          (is (= :invalid-application-handler-system
                 (error-kind #(start nil))))
          (is (= :invalid-application-handler-assembly
                 (error-kind
                  #(start {:biff.ring/handler
                           (valid-lifecycle-handler ctx)}))))
          (is (= :invalid-application-handler-assembly
                 (error-kind
                  #(start
                    {application/application-assembly-system-key
                     {:gesso.live.application-preflight/type
                      :gesso.live.application-preflight/application-assembly}
                     :biff.ring/handler
                     (valid-lifecycle-handler ctx)}))))
          (is (= :missing-biff-ring-handler
                 (error-kind
                  #(start
                    {application/application-assembly-system-key assembly
                     :fixture/value 1}))))
          (doseq [handler [nil 42 :callable-keyword {}]]
            (is (= :invalid-biff-ring-handler
                   (error-kind
                    #(start
                      (application-handler-system
                       assembly
                       handler)))))))))))

(deftest stale-assembly-before-module-installation-fails
  (let [operations (standard-operations)
        ctx (render-context operations)]
    (with-surfaced-application
      {:request-board
       [:main
        (rendered-operation-button
         ctx :request/claim "/operations/request/claim")]}
      (fn [{:keys [assembly artifact-path]}]
        (spit artifact-path "// stale before module installation\n")
        (is (= :invalid-application-handler-assembly
               (error-kind
                #((application-handler-start)
                  (application-handler-system
                   assembly
                   (valid-lifecycle-handler ctx))))))))))

(deftest stale-artifact-after-module-installation-fails-on-next-html-response
  (let [operations (standard-operations)
        ctx (render-context operations)]
    (with-surfaced-application
      {:request-board
       [:main
        (rendered-operation-button
         ctx :request/claim "/operations/request/claim")]}
      (fn [{:keys [assembly artifact-path]}]
        (let [updated
              ((application-handler-start)
               (application-handler-system
                assembly
                (valid-lifecycle-handler ctx)))
              handler (:biff.ring/handler updated)]
          (is (= 200 (:status (handler {:uri "/before"}))))
          (spit artifact-path "// stale after handler installation\n")
          (is (= :rendered-surface-preflight-failed
                 (error-kind #(handler {:uri "/after"})))))))))

(deftest application-handler-module-blocks-route-incoherent-affordance-before-html-delivery
  (let [operations (standard-operations)
        ctx (render-context operations)]
    (with-surfaced-application
      {:request-board
       [:main
        (rendered-operation-button
         ctx :request/claim "/operations/request/claim")]}
      (fn [{:keys [assembly]}]
        (let [handler
              (:biff.ring/handler
               ((application-handler-start)
                (application-handler-system
                 assembly
                 (invalid-lifecycle-handler ctx))))
              data (error-data #(handler {:uri "/"}))]
          (is (= :rendered-surface-preflight-failed (:error/kind data)))
          (is (contains?
               (set (map :kind (:errors (:preflight data))))
               :rendered-affordance-path-mismatch)))))))

(deftest application-handler-module-blocks-silent-semantic-route-downgrade-before-html-delivery
  (with-closed-application
    (fn [{:keys [assembly]}]
      (let [handler
            (:biff.ring/handler
             ((application-handler-start)
              (application-handler-system
               assembly
               (fn [_request]
                 (g/html-response
                  [:main
                   [:button
                    {:hx-post "/operations/request/claim"}
                    "Claim"]])))))
            data (error-data #(handler {:uri "/app"}))]
        (is (= :rendered-surface-preflight-failed (:error/kind data)))
        (is (= #{:rendered-semantic-route-without-choreo-operation}
               (error-kinds (:preflight data))))))))

(deftest later-biff-module-observes-already-wrapped-handler
  (let [operations (standard-operations)
        ctx (render-context operations)]
    (with-surfaced-application
      {:request-board
       [:main
        (rendered-operation-button
         ctx :request/claim "/operations/request/claim")]}
      (fn [{:keys [assembly]}]
        (let [original (valid-lifecycle-handler ctx)
              seen (atom nil)
              observer-id :fixture/observe-wrapped-handler
              observer-module
              {:biff.core/id observer-id
               :biff.core/start
               (fn [system]
                 (reset! seen (:biff.ring/handler system))
                 system)}]
          (with-redefs [module-test-modules
                        [application/application-handler-module
                         observer-module]]
            (let [started
                  (biff/start
                   (application-handler-system assembly original)
                   #'module-test-modules
                   [application/application-handler-module-id
                    observer-id])]
              (is (fn? @seen))
              (is (identical? @seen (:biff.ring/handler started)))
              (is (not (identical? original @seen))))))))))

(deftest duplicate-application-handler-modules-are-rejected-instead-of-composed
  (with-closed-application
    (fn [{:keys [assembly]}]
      (with-redefs [module-test-modules
                    [application/application-handler-module
                     application/application-handler-module]]
        (is (= :missing-or-duplicate-application-handler-module
               (error-kind
                #(application/start-biff-application!
                  assembly
                  {:biff.ring/handler lifecycle-var-handler}
                  #'module-test-modules
                  [application/application-handler-module-id]))))))))

;; =============================================================================
;; v704 released Biff 2 canonical Gesso/Biff startup boundary
;; =============================================================================

(defn canonical-start-module-handler
  [_request]
  (g/html-response [:main [:p "module-handler"]]))

(def canonical-start-modules
  [application/application-handler-module
   {:biff.core/init
    (fn [_modules-var]
      {:biff.ring/handler canonical-start-module-handler})}])

(deftest canonical-biff-start-runs-gesso-before-later-modules
  (let [operations (standard-operations)
        ctx (render-context operations)]
    (with-closed-application
      (fn [{:keys [assembly]}]
        (let [original (valid-lifecycle-handler ctx)
              events (atom [])
              validation-calls (atom 0)
              original-require application/require-rendered-surface!
              observer-id :fixture/observe-canonical-handler
              later-id :fixture/later-module
              observer-module
              {:biff.core/id observer-id
               :biff.core/start
               (fn [system]
                 (swap! events conj :observer)
                 (is (fn? (:biff.ring/handler system)))
                 (is (not (identical? original (:biff.ring/handler system))))
                 (is (not (contains?
                           system
                           application/application-assembly-system-key)))
                 system)}
              later-module
              {:biff.core/id later-id
               :biff.core/start
               (fn [system]
                 (swap! events conj :later)
                 system)}]
          (with-redefs [module-test-modules
                        [application/application-handler-module
                         observer-module
                         later-module]]
            (let [started
                  (application/start-biff-application!
                   assembly
                   {:biff.ring/handler original
                    :fixture/preserved 42}
                   #'module-test-modules
                   [application/application-handler-module-id
                    observer-id
                    later-id])]
              (is (= [:observer :later] @events))
              (is (= 42 (:fixture/preserved started)))
              (with-redefs [application/require-rendered-surface!
                            (fn [app surface rendered]
                              (swap! validation-calls inc)
                              (original-require app surface rendered))]
                (is (= 200
                       (:status
                        ((:biff.ring/handler started) {:uri "/"})))))
              (is (= 1 @validation-calls)))))))))

(deftest canonical-biff-start-short-arity-uses-module-init-handler
  (with-closed-application
    (fn [{:keys [assembly]}]
      (let [started
            (application/start-biff-application!
             assembly
             #'canonical-start-modules
             [application/application-handler-module-id])
            response ((:biff.ring/handler started) {:uri "/module"})]
        (is (fn? (:biff.ring/handler started)))
        (is (= 200 (:status response)))
        (is (re-find #"module-handler" (:body response)))))))

(deftest canonical-biff-start-initial-system-handler-overrides-module-init-handler
  (with-closed-application
    (fn [{:keys [assembly]}]
      (let [initial-handler
            (fn [_request]
              (g/html-response [:main [:p "initial-handler"]]))
            started
            (application/start-biff-application!
             assembly
             {:biff.ring/handler initial-handler
              :fixture/source :initial-system}
             #'canonical-start-modules
             [application/application-handler-module-id])
            response ((:biff.ring/handler started) {:uri "/initial"})]
        (is (= :initial-system (:fixture/source started)))
        (is (= 200 (:status response)))
        (is (re-find #"initial-handler" (:body response)))
        (is (not (re-find #"module-handler" (:body response))))))))

(deftest canonical-biff-start-requires-handler-before-later-server-module
  (with-closed-application
    (fn [{:keys [assembly]}]
      (let [server-ran? (atom false)
            server-id :fixture/install-handler-too-late
            server-module
            {:biff.core/id server-id
             :biff.core/start
             (fn [system]
               (reset! server-ran? true)
               (assoc system
                      :biff.ring/handler
                      canonical-start-module-handler))}]
        (with-redefs [module-test-modules
                      [application/application-handler-module
                       server-module]]
          (is (= :missing-biff-ring-handler
                 (error-kind
                  #(application/start-biff-application!
                    assembly
                    {}
                    #'module-test-modules
                    [application/application-handler-module-id
                     server-id]))))
          (is (false? @server-ran?)))))))

(deftest canonical-biff-start-validates-local-startup-inputs
  (with-closed-application
    (fn [{:keys [assembly]}]
      (is (= :invalid-biff-application-initial-system
             (error-kind
              #(application/start-biff-application!
                assembly
                42
                #'module-test-modules
                [application/application-handler-module-id]))))
      (is (= :invalid-biff-application-modules-var
             (error-kind
              #(application/start-biff-application!
                assembly
                {}
                []
                [application/application-handler-module-id]))))
      (is (= :invalid-biff-application-start-order
             (error-kind
              #(application/start-biff-application!
                assembly
                {}
                #'module-test-modules
                42))))))
  (is (= :invalid-biff-application-start-assembly
         (error-kind
          #(application/start-biff-application!
            nil
            {:biff.ring/handler canonical-start-module-handler}
            #'module-test-modules
            [application/application-handler-module-id])))))

(deftest canonical-biff-start-requires-exactly-one-canonical-gesso-module
  (with-closed-application
    (fn [{:keys [assembly]}]
      (with-redefs [module-test-modules []]
        (is (= :missing-or-duplicate-application-handler-module
               (error-kind
                #(application/start-biff-application!
                  assembly
                  {:biff.ring/handler canonical-start-module-handler}
                  #'module-test-modules
                  [application/application-handler-module-id])))))
      (with-redefs [module-test-modules
                    [application/application-handler-module
                     application/application-handler-module]]
        (is (= :missing-or-duplicate-application-handler-module
               (error-kind
                #(application/start-biff-application!
                  assembly
                  {:biff.ring/handler canonical-start-module-handler}
                  #'module-test-modules
                  [application/application-handler-module-id])))))
      (with-redefs [module-test-modules
                    [{:biff.core/id application/application-handler-module-id
                      :biff.core/start identity}]]
        (is (= :invalid-application-handler-module
               (error-kind
                #(application/start-biff-application!
                  assembly
                  {:biff.ring/handler canonical-start-module-handler}
                  #'module-test-modules
                  [application/application-handler-module-id]))))))))

(deftest canonical-biff-start-requires-gesso-module-first-in-start-order
  (with-closed-application
    (fn [{:keys [assembly]}]
      (let [other-id :fixture/other-lifecycle-module
            other-module
            {:biff.core/id other-id
             :biff.core/start identity}]
        (with-redefs [module-test-modules
                      [application/application-handler-module
                       other-module]]
          (is (= :application-handler-module-not-first
                 (error-kind
                  #(application/start-biff-application!
                    assembly
                    {:biff.ring/handler canonical-start-module-handler}
                    #'module-test-modules
                    [other-id application/application-handler-module-id])))))))))

(deftest canonical-biff-start-rechecks-assembly-currentness-before-start
  (with-closed-application
    (fn [{:keys [assembly artifact-path]}]
      (spit artifact-path "// stale before canonical Biff start\n")
      (is (= :invalid-biff-application-start-assembly
             (error-kind
              #(application/start-biff-application!
                assembly
                {:biff.ring/handler canonical-start-module-handler}
                #'module-test-modules
                [application/application-handler-module-id])))))))

(deftest canonical-biff-start-installed-handler-retains-per-response-currentness
  (let [operations (standard-operations)
        ctx (render-context operations)]
    (with-closed-application
      (fn [{:keys [assembly artifact-path]}]
        (let [started
              (application/start-biff-application!
               assembly
               {:biff.ring/handler (valid-lifecycle-handler ctx)}
               #'module-test-modules
               [application/application-handler-module-id])
              handler (:biff.ring/handler started)]
          (is (= 200 (:status (handler {:uri "/before"}))))
          (spit artifact-path "// stale after canonical Biff start\n")
          (is (= :rendered-surface-preflight-failed
                 (error-kind #(handler {:uri "/after"})))))))))

(deftest canonical-biff-start-accepts-sequential-start-order-and-preserves-order
  (with-closed-application
    (fn [{:keys [assembly]}]
      (let [events (atom [])
            first-id :fixture/first-lifecycle-module
            second-id :fixture/second-lifecycle-module
            first-module
            {:biff.core/id first-id
             :biff.core/start
             (fn [system]
               (swap! events conj :first)
               (assoc system :fixture/first true))}
            second-module
            {:biff.core/id second-id
             :biff.core/start
             (fn [system]
               (swap! events conj :second)
               (assoc system :fixture/second true))}]
        (with-redefs [module-test-modules
                      [application/application-handler-module
                       first-module
                       second-module]]
          (let [started
                (application/start-biff-application!
                 assembly
                 {:biff.ring/handler canonical-start-module-handler
                  :fixture/original :kept}
                 #'module-test-modules
                 (list application/application-handler-module-id
                       first-id
                       second-id))]
            (is (= [:first :second] @events))
            (is (= :kept (:fixture/original started)))
            (is (true? (:fixture/first started)))
            (is (true? (:fixture/second started)))))))))

(deftest later-biff-module-deliberately-replacing-handler-remains-explicit-escape-hatch
  (let [operations (standard-operations)
        ctx (render-context operations)]
    (with-closed-application
      (fn [{:keys [assembly]}]
        (let [original (valid-lifecycle-handler ctx)
              replacement (invalid-lifecycle-handler ctx)
              saw-checked-handler? (atom false)
              replacement-id :fixture/replace-checked-handler
              replacement-module
              {:biff.core/id replacement-id
               :biff.core/start
               (fn [system]
                 (reset!
                  saw-checked-handler?
                  (and (fn? (:biff.ring/handler system))
                       (not (identical?
                             original
                             (:biff.ring/handler system)))))
                 (assoc system :biff.ring/handler replacement))}]
          (with-redefs [module-test-modules
                        [application/application-handler-module
                         replacement-module]]
            (let [started
                  (application/start-biff-application!
                   assembly
                   {:biff.ring/handler original}
                   #'module-test-modules
                   [application/application-handler-module-id
                    replacement-id])]
              (is (true? @saw-checked-handler?))
              (is (identical? replacement (:biff.ring/handler started)))
              ;; Deliberate replacement occurs after the canonical Gesso module,
              ;; so this response is intentionally outside the canonical guarantee.
              (is (= 200
                     (:status
                      ((:biff.ring/handler started)
                       {:uri "/explicit-escape"})))))))))))

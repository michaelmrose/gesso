(ns gesso.assurance.proof.checked-biff-http-application-configuration-test
  "v1807: The actual application startup entrypoint owns module issuance,
   inventory and order; users configure only contract and a selected adapter.
   All passing claims remain bounded to a real local Biff HTTP adapter."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [clojure.string :as str]
            [gesso.assurance.proof.checked-authorization-assembly :as coverage]
            [gesso.assurance.proof.checked-biff-http-application-configuration :as config]
            [gesso.assurance.proof.checked-biff-host-adoption :as host]
            [gesso.assurance.proof.checked-biff-http-consumption-probe :as probe]
            [gesso.assurance.proof.checked-biff-http-module-assembly :as sealed]
            [gesso.assurance.proof.checked-biff-http-adapter-start-test]
            [gesso.assurance.proof.xtdb-gateway-issued-catalog-canonical-trial-test
             :as previous]
            [gesso.live.application-preflight.fixture :as app-fixture]
            [gesso.model.command-resolution :as resolution]
            [gesso.model.resolved-operation-test])
  (:import [com.sun.net.httpserver HttpHandler HttpServer]
           [java.net BindException InetSocketAddress URI]
           [java.net.http HttpClient HttpRequest HttpRequest$BodyPublishers
            HttpResponse$BodyHandlers]
           [java.nio.charset StandardCharsets]
           [java.time Duration]
           [java.util.concurrent Executors TimeUnit]))

(def ^:private scenario-ns
  'gesso.assurance.proof.checked-biff-http-adapter-start-test)
(def ^:private model-ns 'gesso.model.resolved-operation-test)
(defn- helper [ns-name s]
  (var-get (or (ns-resolve ns-name s)
               (throw (ex-info "Missing original test fixture"
                               {:ns ns-name :name s})))))
(defn- model [s] (helper model-ns s))
(defn- scenario [mode] ((helper scenario-ns 'scenario) mode false))
(use-fixtures :once
  (fn [run]
    ((model 'with-real-xtdb)
     (fn []
       (app-fixture/with-closed-application
        {:operations {:request/claim
                      (app-fixture/trusted-operation
                       :request/claim {:published-change-topics #{}})}}
        (fn [{:keys [assembly]}]
          (binding [previous/*assembly* assembly] (run))))))))

(defn- original-source [s & [typed?]]
  (cond-> {:contract-source (get-in s [:env :contract])
           :adapter-module (:raw s)
           :base-url! (fn [_] (str "http://127.0.0.1:"
                                   (:port @(:server-state s))))}
    typed?
    (assoc :typed-trial
           {:catalog-source (get-in s [:env :catalog-source])
            :operation :request/claim
            :inputs {:input/owner "helper-a"}})))

(defn- reason [f]
  (try (f) :no-error
       (catch clojure.lang.ExceptionInfo ex (:reason (ex-data ex)))))
(defn- post! [s]
  (let [port (:port @(:server-state s))
        req (-> (HttpRequest/newBuilder
                 (URI/create (str "http://127.0.0.1:" port
                                  "/operations/request/claim")))
                (.timeout (Duration/ofSeconds 6))
                (.POST (HttpRequest$BodyPublishers/noBody))
                (.build))]
    (.statusCode (.send (HttpClient/newHttpClient) req
                        (HttpResponse$BodyHandlers/ofString)))))
(defn- cleanup! [s issued]
  ((:stop! issued))
  ;; The raw adapter's stop is also idempotent in this retained test fixture.
  (when-let [stop (get-in s [:raw :biff.core/stop])]
    (stop {})))

(deftest application-config-start-derives-sealed-inventory-and-real-http-adoption
  (let [s (scenario :honest)
        {:keys [request binding]} (get-in s [:env :data])
        issued (config/issue-configured-checked-http-application!
                (original-source s))]
    ((model 'seed!) [:requests request])
    (try
      (is (= :issued (:status ((:summary! issued)))))
      (let [result ((:start-once! issued) {})
            report (:report result)]
        (is (= config/report-type (:type report)))
        (is (= :verified-configured-start (:status report)))
        (is (= :internally-derived-original-four-modules
               (:module-inventory report)))
        (is (= :internally-derived-canonical (:module-order report)))
        (is (= :selected-adapter-challenge-passed-during-start
               (:real-http-consumption report)))
        (is (= :not-established (:declared-route-coverage-at-start report)))
        (is (= :not-established (:returned-host-selected-route-inventory report)))
        (is (= :not-established (:selected-catalog-read-only-http-dispatch report)))
        (is (= :not-established (:external-deployment report)))
        (is (= :none (:authority report) (:retry-authority report)))
        (is (not-any? #(contains? report %)
                      [:modules :host-handle :probe :adapter :handler :source
                       :contract :system :tx-ops :receipt]))
        (is (= request ((model 'read-request) (:xt/id request))))
        (is (nil? ((model 'read-command) (resolution/record-id binding))))
        (is (= 200 (post! s)))
        (is (some? ((model 'read-command) (resolution/record-id binding)))))
      (finally (cleanup! s issued)))
    (is (= :stopped (:status ((:summary! issued)))))))

(deftest configured-start-preserves-typed-atomic-post-through-actual-adapter
  (let [s (scenario :honest)
        {:keys [request binding]} (get-in s [:env :data])
        issued (config/issue-configured-checked-http-application!
                (original-source s true))]
    ((model 'seed!) [:requests request])
    (try
      (let [report (:report ((:start-once! issued) {}))]
        (is (= :verified-configured-start (:status report)))
        (is (= :selected-certified-catalog-routes-source-checked-before-start
               (:declared-route-coverage-at-start report)))
        (is (= :exact-selected-certified-route-set-observed-beneath-final-probe-wrapper
               (:returned-host-selected-route-inventory report)))
        (is (= :all-selected-certified-routes-observed-before-start-return
               (:selected-catalog-read-only-http-dispatch report)))
        (is (= :not-established (:parameterized-route-generalization report)))
        (is (= :not-established (:unprobed-routes-over-real-http report)))
        (is (= :not-established
               (:selected-catalog-business-policy-completeness report))))
      (is (= 200 (post! s)))
      (is (= ((model 'next-version) request {:owner "helper-a"})
             ((model 'read-request) (:xt/id request))))
      (is (some? ((model 'read-command) (resolution/record-id binding))))
      (is (= 500 (post! s)))
      (is (= 1 @(get-in s [:env :environment :constructs])))
      (finally (cleanup! s issued)))))

(deftest invalid-application-configuration-cannot-inject-module-var-or-order
  (let [s (scenario :honest)
        basic (original-source s)]
    (doseq [extra [{:modules-var #'config/issue-configured-checked-http-application!}
                   {:start-order [:untrusted/raw-adapter]}
                   {:server-module (:raw s)}
                   {:certificate :synthetic}]]
      (is (= :invalid-application-owned-http-configuration
             (reason #(config/issue-configured-checked-http-application!
                       (merge basic extra))))))
    (is (= :invalid-application-owned-http-configuration
           (reason #(config/issue-configured-checked-http-application!
                     (assoc basic :typed-trial
                            {:operation :request/claim
                             :inputs {:input/owner "helper-a"}
                             :submit! (fn [] :forged)})))))
    (is (nil? @(:server-state s)))))

(deftest unexpected-preinstalled-biff-handler-rejected-before-server-start
  (let [s (scenario :honest)
        issued (config/issue-configured-checked-http-application!
                (original-source s))]
    (is (= :invalid-or-preinstalled-biff-initial-system
           (reason #((:start-once! issued)
                     {:biff.ring/handler (fn [_] {:status 200})}))))
    (is (nil? @(:server-state s)))
    (is (= :start-failed-physical-state-unknown
           (:status ((:summary! issued)))))
    (is (= :configured-start-already-attempted
           (reason #((:start-once! issued) {}))))
    (cleanup! s issued)))

(deftest configured-start-rejects-selected-adapter-ignoring-checked-handler
  (let [s (scenario :ignored)
        issued (config/issue-configured-checked-http-application!
                (original-source s))]
    (is (= :adapter-start-http-consumption-unverified
           (reason #((:start-once! issued) {}))))
    (is (= :start-failed-physical-state-unknown
           (:status ((:summary! issued)))))
    (is (.isShutdown (:executor @(:server-state s))))
    (is (= :configured-start-already-attempted
           (reason #((:start-once! issued) {}))))
    (cleanup! s issued)))

(deftest invalid-local-consumption-target-blocks-selected-server-start
  (let [s (scenario :honest)
        issued (config/issue-configured-checked-http-application!
                (assoc (original-source s)
                       :base-url! (fn [_] "http://example.com:80")))]
    (is (= :nonloopback-or-unsafe-probe-target
           (reason #((:start-once! issued) {}))))
    (is (.isShutdown (:executor @(:server-state s))))
    (is (= :start-failed-physical-state-unknown
           (:status ((:summary! issued)))))
    (cleanup! s issued)))

(deftest configured-start-is-once-only-and-stop-is-idempotent
  (let [s (scenario :honest)
        issued (config/issue-configured-checked-http-application!
                (original-source s))]
    (try
      ((:start-once! issued) {})
      (is (= :configured-start-already-attempted
             (reason #((:start-once! issued) {}))))
      (is (= :stopped (:status ((:stop! issued)))))
      (is (= :stopped (:status ((:stop! issued)))))
      (finally (cleanup! s issued)))))

(deftest stop-before-start-refuses-to-resurrect-adapter
  (let [s (scenario :honest)
        issued (config/issue-configured-checked-http-application!
                (original-source s))]
    (is (= :stopped-without-successful-start (:status ((:stop! issued)))))
    (is (= :configured-start-stopped-before-use
           (reason #((:start-once! issued) {}))))
    (is (nil? @(:server-state s)))
    (cleanup! s issued)))

(deftest substituted-sealed-start-boundary-rejects-before-server-initialization
  (let [s (scenario :honest)
        issued (config/issue-configured-checked-http-application!
                (original-source s))]
    (with-redefs [sealed/start-closed-checked-http-application!
                  (fn [& _] {:system {} :report {:physical-server-start
                                                  :one-selected-verified-adapter-start}})]
      (is (= :substituted-original-configured-start-boundary
             (reason #((:start-once! issued) {})))))
    (is (nil? @(:server-state s)))
    (cleanup! s issued)))

(deftest stopping-an-in-progress-start-cannot-claim-successful-cleanup
  (let [s (scenario :honest)
        entered (promise)
        continue (promise)
        source (assoc (original-source s)
                      :base-url! (fn [system]
                                   (deliver entered true)
                                   (when-not (deref continue 8000 false)
                                     (throw (ex-info "unreleased adapter" {})))
                                   ((:base-url! (original-source s)) system)))
        issued (config/issue-configured-checked-http-application! source)
        started (future
                  (try ((:start-once! issued) {})
                       (catch Throwable e e)))]
    (try
      (is (= true (deref entered 10000 false)))
      (is (= :configured-start-in-progress (reason (:stop! issued))))
      (is (= :starting (:status ((:summary! issued)))))
      (deliver continue true)
      (let [result (deref started 10000 :timeout)]
        (is (map? result))
        (is (= :verified-configured-start (get-in result [:report :status])))
        (is (= :stopped (:status ((:stop! issued))))))
      (finally
        (deliver continue true)
        (future-cancel started)
        (cleanup! s issued)))))


(deftest selected-catalog-coverage-refuses-incomplete-declarations-before-biff-start
  (let [s (scenario :honest)
        original (original-source s true)
        catalog (get-in original [:typed-trial :catalog-source])
        claim (first (get-in catalog [:catalog :operations]))
        other (assoc claim :id :request/other)
        bad-sources [(assoc-in original [:typed-trial :catalog-source
                                         :catalog :operations] [])
                     (assoc-in original [:typed-trial :catalog-source
                                         :catalog :operations] [claim other])
                     (assoc-in original [:typed-trial :catalog-source
                                         :catalog :operations] [claim claim])]]
    (doseq [bad bad-sources]
      (let [issued (config/issue-configured-checked-http-application! bad)]
        (try
          (is (= :selected-catalog-route-coverage-not-checked
                 (reason #((:start-once! issued) {}))))
          (is (nil? @(:server-state s)))
          (is (= :configured-start-already-attempted
                 (reason #((:start-once! issued) {}))))
          (finally (cleanup! s issued)))))))

(deftest selected-catalog-coverage-checker-substitution-blocks-real-start
  (let [s (scenario :honest)
        issued (config/issue-configured-checked-http-application!
                (original-source s true))]
    (try
      (with-redefs [coverage/check-declared-route-coverage!
                    (fn [_] {:type coverage/coverage-report-type
                             :checking :all-declared-catalog-operations-and-issued-routes})]
        (is (= :substituted-original-configured-start-boundary
               (reason #((:start-once! issued) {})))))
      (is (nil? @(:server-state s)))
      (finally (cleanup! s issued)))))


(deftest selected-host-route-inventory-must-match-declared-catalog-exactly
  ;; These isolated reports test only the structural comparison predicate.
  ;; Real report authenticity is established by the original host handle and
  ;; returned system in the configured start, not by these plain maps.
  (let [selected {:type coverage/coverage-report-type
                  :version coverage/report-version
                  :current-assembly-closure :source-relative-checked
                  :matched-certified-route-ids #{:request/claim :request/finish}}
        observed {:type probe/final-catalog-dispatch-report-type
                  :version probe/report-version
                  :module-id probe/module-id
                  :host-module-id host/module-id
                  :scope :one-original-probe-one-returned-system-and-host
                  :final-probe-handler-slot :exact-issued-wrapper
                  :installed-handler-adoption :one-returned-in-process-probe-handler-slot
                  :normal-http-route-inventory :checked-and-selected-dispatch-witnessed
                  :checked-route-ids #{:request/claim :request/finish :page/home}
                  :checked-certified-route-ids #{:request/claim :request/finish}
                  :external-deployment :not-established
                  :certificate :not-issued :admission :not-admitted
                  :transaction-authority :none :authority :none}
        check! (var-get #'config/require-matching-selected-host-routes!)]
    (is (true? (check! selected observed)))
    (doseq [wrong [(assoc observed :checked-certified-route-ids
                          #{:request/claim})
                   (assoc observed :checked-certified-route-ids
                          #{:request/claim :request/finish :request/extra})
                   (assoc observed :checked-route-ids #{:request/claim})
                   (assoc observed :final-probe-handler-slot :untrusted-replacement)
                   (assoc observed :installed-handler-adoption
                          :not-established)
                   (assoc observed :type :synthetic/proof)
                   (assoc observed :authority :issued)
                   (assoc observed :external-deployment :certified)
                   (dissoc observed :checked-route-ids)]]
      (is (= :selected-catalog-route-inventory-not-adopted
             (reason #(check! selected wrong)))
          (pr-str wrong)))
    (is (= :selected-catalog-route-inventory-not-adopted
           (reason #(check! (assoc selected :matched-certified-route-ids
                                  #{:request/claim}) observed))))))

(deftest selected-route-check-failure-after-http-start-stops-the-adapter
  ;; Inject a failure AFTER the real sealed adapter has returned, to test
  ;; cleanup of the selected already-open listener. This deliberately does
  ;; not mock the original host's source/identity evidence.
  (let [s (scenario :honest)
        issued (config/issue-configured-checked-http-application!
                (original-source s true))
        check-var #'config/require-matching-selected-host-routes!]
    (try
      (is (= :selected-catalog-route-inventory-not-adopted
             (with-redefs-fn
               {check-var (fn [& _]
                            (throw (ex-info "host mismatch"
                                            {:reason :selected-catalog-route-inventory-not-adopted})))}
               #(reason (fn [] ((:start-once! issued) {}))))))
      (is (some? @(:server-state s)))
      (is (.isShutdown (:executor @(:server-state s))))
      (is (= :start-failed-physical-state-unknown
             (:status ((:summary! issued)))))
      (is (= :configured-start-already-attempted
             (reason #((:start-once! issued) {}))))
      (finally (cleanup! s issued)))))


(deftest selected-route-observation-report-must-be-complete-and-source-aligned
  ;; Structural regression, not a substitute for the issued probe's genuine
  ;; capability and actual HTTP challenge. Those are checked in start!.
  (let [selected {:matched-certified-route-ids #{:request/claim :request/finish}}
        good {:type probe/final-catalog-dispatch-report-type
              :version probe/report-version
              :scope :one-original-probe-one-returned-system-and-host
              :catalog-read-only-route-dispatch
              :all-selected-certified-routes-observed-through-original-router
              :catalog-expected-certified-route-ids #{:request/claim :request/finish}
              :catalog-observed-certified-route-ids #{:request/claim :request/finish}
              :selected-route-challenge-observations 2
              :selected-route-business-handler-execution :not-invoked-by-read-only-probe
              :catalog-parameterized-witness-route-ids #{:request/finish}
              :parameterized-route-generalization :not-established
              :typed-trial-attempts 0 :authority :none :transaction-authority :none
              :retry-authority :none :certificate :not-issued :admission :not-admitted}
        check! (var-get #'config/require-selected-catalog-dispatch!)]
    (is (true? (check! selected good)))
    (doseq [bad [(assoc good :catalog-observed-certified-route-ids #{:request/claim})
                 (assoc good :catalog-expected-certified-route-ids #{:request/claim})
                 (assoc good :selected-route-challenge-observations 1)
                 (assoc good :selected-route-challenge-observations 3)
                 (assoc good :catalog-read-only-route-dispatch :not-established)
                 (assoc good :scope :synthetic/detached)
                 (assoc good :typed-trial-attempts 1)
                 (assoc good :catalog-parameterized-witness-route-ids #{:request/other})
                 (assoc good :parameterized-route-generalization :proved)
                 (assoc good :selected-route-business-handler-execution :executed)
                 (assoc good :type :synthetic/report)
                 (assoc good :authority :issued)
                 (assoc good :certificate :forged)]]
      (is (= :selected-catalog-http-dispatch-not-observed
             (reason #(check! selected bad))) (pr-str bad)))
    ;; A detached map with an issuer-like callback is not an original source
    ;; result; the original probe check must precede this structural check.
    (is (= :selected-catalog-http-dispatch-not-observed
           (reason #(check! selected {:summary! (fn [] good)}))))))

(deftest incomplete-route-observation-after-server-start-stops-selected-adapter
  ;; Deliberately fail the final report qualification AFTER the sealed
  ;; application and real HTTP server have started. This tests cleanup and
  ;; rejection rather than pretending to manufacture a real issued report.
  (let [s (scenario :honest)
        issued (config/issue-configured-checked-http-application!
                (original-source s true))
        check-var #'config/require-selected-catalog-dispatch!]
    (try
      (is (= :selected-catalog-http-dispatch-not-observed
             (with-redefs-fn
               {check-var (fn [& _]
                            (throw (ex-info "unobserved certified route"
                                            {:reason :selected-catalog-http-dispatch-not-observed})))}
               #(reason (fn [] ((:start-once! issued) {}))))))
      (is (some? @(:server-state s)))
      (is (.isShutdown (:executor @(:server-state s))))
      (is (= :start-failed-physical-state-unknown
             (:status ((:summary! issued)))))
      (is (= :configured-start-already-attempted
             (reason #((:start-once! issued) {}))))
      (finally (cleanup! s issued)))))


(deftest configured-managed-instance-inspection-is-current-only
  (let [s (scenario :honest)
        {:keys [request]} (get-in s [:env :data])
        issued (config/issue-configured-checked-http-application!
                (original-source s true))]
    ((model 'seed!) [:requests request])
    (try
      (is (= :configured-instance-not-current
             (reason #((:inspect-running! issued) {}))))
      (let [{:keys [system report]} ((:start-once! issued) {})
            inspect! (:inspect-running! issued)
            observed (inspect! system)]
        (is (= :verified-configured-start (:status report)))
        (is (= config/running-report-type (:type observed)))
        (is (= :current-at-inspection (:status observed)))
        (is (= :one-original-managed-in-process-biff-instance (:scope observed)))
        (is (= :exact-identity (:returned-system observed)))
        (is (= :verified-not-stopped (:original-guarded-adapter observed)))
        (is (= :exact-issued-wrapper (:original-probe-handler-slot observed)))
        (is (= :matches-current-original-host-route-set
               (:selected-catalog-route-continuity observed)))
        (is (= :not-established (:external-listener-liveness observed)))
        (is (= :not-established (:out-of-band-adapter-stops observed)))
        (is (= :observed-at-current-inspection
               (:fresh-local-http-response observed)))
        (is (= :not-established
               (:future-local-listener-availability observed)))
        (is (= :none (:authority observed) (:transaction-authority observed)))
        (is (not-any? #(contains? observed %)
                      [:system :handler :host-handle :probe :source
                       :catalog :biff.ring/handler :port :url :token]))
        (is (= :configured-instance-not-current
               (reason #(inspect! (assoc system :unrelated :copy)))))
        ;; Startup-only route dispatch evidence cannot be re-run after a
        ;; successful typed command because the command's attempt counter
        ;; has legitimately advanced. Current lifecycle/host identity can.
        (is (= 200 (post! s)))
        (is (= :current-at-inspection (:status (inspect! system))))
        (is (= :observed-at-current-inspection
               (:fresh-local-http-response (inspect! system))))
        (is (= :matches-current-original-host-route-set
               (:selected-catalog-route-continuity (inspect! system))))
        (is (= :stopped (:status ((:stop! issued)))))
        (is (= :configured-instance-not-current
               (reason #(inspect! system))))
        (is (= :configured-start-already-attempted
               (reason #((:start-once! issued) {}))))
        (is (= :requires-current-instance-inspection
               (:managed-instance-liveness ((:summary! issued))))))
      (finally (cleanup! s issued)))))

(deftest unselected-configured-instance-inspection-stops-with-adapter
  (let [s (scenario :honest)
        issued (config/issue-configured-checked-http-application!
                (original-source s))]
    (try
      (let [{:keys [system]} ((:start-once! issued) {})]
        (is (= :current-at-inspection
               (:status ((:inspect-running! issued) system))))
        (is (= :not-selected
               (:selected-catalog-source ((:inspect-running! issued) system))))
        (is (= :not-selected
               (:selected-catalog-route-continuity
                ((:inspect-running! issued) system))))
        (is (= :observed-at-current-inspection
               (:fresh-local-http-response ((:inspect-running! issued) system))))
        (is (= :stopped (:status ((:stop! issued)))))
        (is (= :configured-instance-not-current
               (reason #((:inspect-running! issued) system)))))
      (finally (cleanup! s issued)))))

(deftest uncertain-guarded-adapter-stop-cannot-be-reported-as-clean
  (let [s (scenario :honest)
        original-stop (get-in s [:raw :biff.core/stop])
        stopping (assoc (:raw s) :biff.core/stop
                        (fn [system]
                          ;; Real listener cleanup runs, but the callback
                          ;; subsequently raises. Its completion is unknown
                          ;; to the checked guarded adapter.
                          (original-stop system)
                          (throw (ex-info "stop completion uncertain" {}))))
        issued (config/issue-configured-checked-http-application!
                (assoc (original-source s)
                       :adapter-module stopping))]
    (try
      (let [{:keys [system]} ((:start-once! issued) {})]
        (is (= :current-at-inspection
               (:status ((:inspect-running! issued) system))))
        (is (= :stop-cleanup-not-established
               (:status ((:stop! issued)))))
        (is (= :configured-instance-not-current
               (reason #((:inspect-running! issued) system))))
        (is (.isShutdown (:executor @(:server-state s))))
        (is (= :stop-cleanup-not-established
               (:status ((:summary! issued))))))
      (finally (cleanup! s issued)))))

(deftest current-instance-inspection-does-not-overlap-managed-shutdown
  ;; Block the real raw adapter stop after the managed lifecycle has entered
  ;; :stopping. The inspection must remain pending until the ORIGINAL Biff
  ;; stop returns, not merely race a state check while cleanup is underway.
  (let [s (scenario :honest)
        raw-stop (get-in s [:raw :biff.core/stop])
        stop-entered (promise)
        allow-stop (promise)
        adapter (assoc (:raw s)
                       :biff.core/stop
                       (fn [system]
                         (deliver stop-entered true)
                         (when-not (deref allow-stop 15000 false)
                           (throw (ex-info "stop fixture timed out" {})))
                         (raw-stop system)))
        issued (config/issue-configured-checked-http-application!
                (assoc (original-source s true) :adapter-module adapter))]
    (try
      (let [{:keys [system]} ((:start-once! issued) {})
            stopping (future ((:stop! issued)))]
        (try
          (is (true? (deref stop-entered 15000 false)))
          (let [inspection-entered (promise)
                inspection (future
                             (deliver inspection-entered true)
                             (reason #((:inspect-running! issued) system)))]
            (is (true? (deref inspection-entered 5000 false)))
            ;; Without the shared lifecycle lock the inspection immediately
            ;; rejects while the stop callback is blocked, making the
            ;; observer and physical stop operations overlap in time.
            (is (= :inspection-still-pending
                   (deref inspection 250 :inspection-still-pending)))
            (deliver allow-stop true)
            (is (= :stopped (:status (deref stopping 15000 :timeout))))
            (is (= :configured-instance-not-current
                   (deref inspection 15000 :timeout)))
            (is (= :configured-instance-not-current
                   (reason #((:inspect-running! issued) system))))
            (is (= :configured-start-already-attempted
                   (reason #((:start-once! issued) {})))))
          (finally
            (deliver allow-stop true)
            (deref stopping 15000 :timeout))))
      (finally
        (deliver allow-stop true)
        (cleanup! s issued)))))

(deftest running-selected-route-continuity-demands-exact-original-host-set
  ;; The actual current host inventory is authenticated by the original
  ;; issued probe in inspect-running!. These plain maps test only the
  ;; secondary shape/equality predicate, never evidence authenticity.
  (let [check! (var-get #'config/require-running-selected-route-continuity!)
        routes #{:request/claim :request/finish}
        inventory {:checked-certified-route-ids routes
                   :checked-route-ids (conj routes :page/home)}]
    (is (true? (check! true routes inventory)))
    (is (true? (check! false nil inventory)))
    (doseq [[selected? saved current]
            [[true routes (assoc inventory :checked-certified-route-ids
                                 #{:request/claim})]
             [true routes (assoc inventory :checked-certified-route-ids
                                 (conj routes :request/other))]
             [true routes (assoc inventory :checked-route-ids
                                 #{:request/claim :page/home})]
             [true routes (dissoc inventory :checked-route-ids)]
             [true routes (assoc inventory :checked-certified-route-ids
                                 [:request/claim :request/finish])]
             [true nil inventory]
             [true #{} inventory]
             [true #{:request/claim "request/finish"} inventory]
             [false routes inventory]]]
      (is (= :configured-instance-selected-routes-not-current
             (reason #(check! selected? saved current)))
          (pr-str {:selected? selected? :saved saved :current current})))))


(deftest current-instance-detects-out-of-band-listener-shutdown
  ;; An out-of-band raw adapter stop is not tracked by the managed state,
  ;; but a fresh private loopback HTTP challenge MUST fail instead of
  ;; promoting that old managed status to present-tense liveness.
  (let [s (scenario :honest)
        issued (config/issue-configured-checked-http-application!
                (original-source s true))]
    (try
      (let [{:keys [system]} ((:start-once! issued) {})]
        (is (= :observed-at-current-inspection
               (:fresh-local-http-response ((:inspect-running! issued) system))))
        ((get-in s [:raw :biff.core/stop]) system)
        (is (= :configured-instance-http-liveness-not-observed
               (reason #((:inspect-running! issued) system))))
        (is (= :verified-configured-start
               (:status ((:summary! issued))))
            "Historical managed startup status cannot prove current listener response"))
      (finally (cleanup! s issued)))))

(deftest substituted-liveness-checker-cannot-authorize-current-instance
  (let [s (scenario :honest)
        issued (config/issue-configured-checked-http-application!
                (original-source s true))]
    (try
      (let [{:keys [system]} ((:start-once! issued) {})]
        (with-redefs [probe/require-fresh-local-http-liveness!
                      (fn [& _] {:type probe/fresh-local-http-liveness-report-type
                                 :status :responded-to-current-inspection-challenge})]
          (is (= :substituted-original-configured-start-boundary
                 (reason #((:inspect-running! issued) system)))))
        (is (= :observed-at-current-inspection
               (:fresh-local-http-response ((:inspect-running! issued) system)))))
      (finally (cleanup! s issued)))))


(deftest started-instance-generation-requires-exact-issued-identity
  ;; A redacted status or a caller-constructed map is not the opaque started
  ;; generation. The new observation remains a fresh read-only HTTP check.
  (let [s (scenario :honest)
        {:keys [request]} (get-in s [:env :data])
        issued (config/issue-configured-checked-http-application!
                (original-source s true))
        inspect! (:inspect-generation! issued)]
    ((model 'seed!) [:requests request])
    (try
      (is (= :configured-instance-generation-not-current
             (reason #(inspect! (Object.) {}))))
      (let [{:keys [system report instance-generation]}
            ((:start-once! issued) {})
            observed (inspect! instance-generation system)]
        (is (= :verified-configured-start (:status report)))
        (is (some? instance-generation))
        (is (= "#<gesso.assurance/started-instance-generation>"
               (str instance-generation)))
        (is (= config/running-report-type (:type observed)))
        (is (= :current-at-inspection (:status observed)))
        (is (= :exact-original-issued-started-instance
               (:generation-binding observed)))
        (is (= :observed-at-current-inspection
               (:fresh-local-http-response observed)))
        (is (not-any? #(contains? observed %)
                      [:instance-generation :generation :system :handler
                       :port :url :nonce :token]))
        (doseq [wrong [(Object.) report observed
                       {:type config/running-report-type
                        :generation-binding :exact-original-issued-started-instance}]]
          (is (= :configured-instance-generation-not-current
                 (reason #(inspect! wrong system)))))
        (is (= :configured-instance-generation-not-current
               (reason #(inspect! instance-generation
                                  (assoc system :copied true)))))
        (is (= 200 (post! s)))
        (is (= :exact-original-issued-started-instance
               (:generation-binding (inspect! instance-generation system))))
        (is (= :stopped (:status ((:stop! issued)))))
        (is (= :configured-instance-generation-not-current
               (reason #(inspect! instance-generation system)))))
      (finally (cleanup! s issued)))))

(deftest independent-started-generations-cannot-be-cross-adopted
  ;; Two independently issued adapters may expose identical application
  ;; route sets and report shapes. They MUST NOT exchange the local, opaque
  ;; started-generation or exact returned Biff-system identity witnesses.
  (let [s1 (scenario :honest)
        s2 (scenario :honest)
        a (config/issue-configured-checked-http-application!
           (original-source s1 true))
        b (config/issue-configured-checked-http-application!
           (original-source s2 true))]
    (try
      (let [first-start ((:start-once! a) {})
            second-start ((:start-once! b) {})
            first-gen (:instance-generation first-start)
            second-gen (:instance-generation second-start)
            first-system (:system first-start)
            second-system (:system second-start)
            inspect-a (:inspect-generation! a)
            inspect-b (:inspect-generation! b)]
        (is (not (identical? first-gen second-gen)))
        (is (not (identical? first-system second-system)))
        (is (= :exact-original-issued-started-instance
               (:generation-binding (inspect-a first-gen first-system))))
        (is (= :exact-original-issued-started-instance
               (:generation-binding (inspect-b second-gen second-system))))
        (doseq [[inspect! generation system]
                [[inspect-a second-gen first-system]
                 [inspect-b first-gen second-system]
                 [inspect-a first-gen second-system]
                 [inspect-b second-gen first-system]]]
          (is (= :configured-instance-generation-not-current
                 (reason #(inspect! generation system)))))
        ;; Even after the first instance stops, the second remains current.
        (is (= :stopped (:status ((:stop! a)))))
        (is (= :configured-instance-generation-not-current
               (reason #(inspect-a first-gen first-system))))
        (is (= :exact-original-issued-started-instance
               (:generation-binding (inspect-b second-gen second-system)))))
      (finally
        (cleanup! s1 a)
        (cleanup! s2 b)))))


(deftest original-listener-endpoint-survives-repointed-locator-and-independent-adapter
  ;; Both instances start on distinct real loopback ports. Redirecting the
  ;; first instance's mutable locator to the second instance must NOT
  ;; let the first one's generation qualify.
  (let [first-s (scenario :honest)
        second-s (scenario :honest)
        first-source (original-source first-s true)
        first-locator (:base-url! first-source)
        redirected (atom nil)
        first-issued
        (config/issue-configured-checked-http-application!
         (assoc first-source :base-url!
                (fn [system] (or @redirected (first-locator system)))))
        second-issued
        (config/issue-configured-checked-http-application!
         (original-source second-s true))]
    (try
      (let [first-start ((:start-once! first-issued) {})
            second-start ((:start-once! second-issued) {})
            first-system (:system first-start)
            first-generation (:instance-generation first-start)
            second-system (:system second-start)
            second-generation (:instance-generation second-start)
            inspect-first (fn []
                            ((:inspect-generation! first-issued)
                             first-generation first-system))
            inspect-second (fn []
                             ((:inspect-generation! second-issued)
                              second-generation second-system))
            second-url (str "http://127.0.0.1:"
                            (:port @(:server-state second-s)))]
        (is (not= (:port @(:server-state first-s))
                  (:port @(:server-state second-s))))
        (is (= :matches-startup-verified-loopback-port
               (:original-local-listener-endpoint (inspect-first))))
        (reset! redirected second-url)
        (is (= :configured-instance-http-liveness-not-observed
               (reason inspect-first)))
        (is (= :exact-original-issued-started-instance
               (:generation-binding (inspect-second))))
        (reset! redirected nil)
        (is (= :exact-original-issued-started-instance
               (:generation-binding (inspect-first))))
        (is (= :stopped (:status ((:stop! first-issued)))))
        (is (= :configured-instance-generation-not-current
               (reason inspect-first)))
        (is (= :matches-startup-verified-loopback-port
               (:original-local-listener-endpoint (inspect-second)))))
      (finally
        (cleanup! first-s first-issued)
        (cleanup! second-s second-issued)))))


(deftest trusted-physical-listener-reference-stays-bound-to-original-instance
  ;; A source-trusted callback returns the selected adapter's actual JDK
  ;; HttpServer. Changing the reported object at the same port rejects
  ;; original instance identity, even while the original socket still serves.
  (let [s (scenario :honest)
        source (original-source s true)
        alternate (atom nil)
        issued (config/issue-configured-checked-http-application!
                (assoc source :listener-instance!
                       (fn [_]
                         (or @alternate (:server @(:server-state s))))))]
    (try
      (let [{:keys [system instance-generation]} ((:start-once! issued) {})
            inspect! (fn []
                       ((:inspect-generation! issued)
                        instance-generation system))
            initial (inspect!)
            original-server (:server @(:server-state s))
            original-port (:port @(:server-state s))]
        (is (some? original-server))
        (is (= :matches-original-host-reported-object
               (:physical-listener-instance initial)))
        (is (= :trusted-host-callback-conditional
               (:physical-instance-provenance initial)))
        (is (= :matches-startup-verified-loopback-port
               (:original-local-listener-endpoint initial)))
        (is (= :exact-original-issued-started-instance
               (:generation-binding initial)))
        (is (not-any? #(contains? initial %)
                      [:listener-instance! :listener-instance :server
                       :listener-handle :source :url :port :generation
                       :instance-generation]))
        (reset! alternate (Object.))
        (is (= original-port (:port @(:server-state s))))
        (is (= :configured-physical-listener-not-current
               (reason inspect!)))
        (is (= :configured-physical-listener-not-current
               (reason #((:inspect-running! issued) system))))
        (reset! alternate nil)
        (is (= :matches-original-host-reported-object
               (:physical-listener-instance (inspect!))))
        (is (= :stopped (:status ((:stop! issued)))))
        (is (= :configured-instance-generation-not-current
               (reason inspect!))))
      (finally (cleanup! s issued)))))

(deftest physical-listener-identity-is-optional-and-explicitly-unestablished
  (let [s (scenario :honest)
        issued (config/issue-configured-checked-http-application!
                (original-source s true))]
    (try
      (let [{:keys [system]} ((:start-once! issued) {})
            observed ((:inspect-running! issued) system)]
        (is (= :not-established (:physical-listener-instance observed)))
        (is (= :not-established (:physical-instance-provenance observed))))
      (finally (cleanup! s issued)))))

(deftest invalid-physical-listener-callback-fails-closed-and-cleans-up
  (let [s (scenario :honest)
        source (original-source s)]
    (is (= :invalid-application-owned-http-configuration
           (reason #(config/issue-configured-checked-http-application!
                     (assoc source :listener-instance! :synthetic/not-a-callback)))))
    (is (nil? @(:server-state s)))
    (let [issued (config/issue-configured-checked-http-application!
                  (assoc source :listener-instance!
                         (fn [_] {:server :fabricated})))]
      (try
        (is (= :configured-physical-listener-not-current
               (reason #((:start-once! issued) {}))))
        (is (some? @(:server-state s)))
        (is (.isShutdown (:executor @(:server-state s))))
        (is (= :start-failed-physical-state-unknown
               (:status ((:summary! issued)))))
        (is (= :configured-start-already-attempted
               (reason #((:start-once! issued) {}))))
        (finally (cleanup! s issued))))))

;; v1807: A second, genuinely bound JDK HttpServer replaces the first on the
;; SAME loopback port. Reusing its checked Ring wrapper is intentional: a
;; fresh private HTTP nonce alone cannot identify the physical server object.
(defn- bind-replacement-on-port! [port checked-ring-handler]
  (let [address (InetSocketAddress. "127.0.0.1" (int port))
        server (loop [attempt 0]
                 (let [outcome (try
                                 {:server (HttpServer/create address 0)}
                                 (catch BindException ex {:error ex}))]
                   (if-let [bound (:server outcome)]
                     bound
                     (if (< attempt 9)
                       (do (Thread/sleep 30) (recur (inc attempt)))
                       (throw (:error outcome))))))
        executor (Executors/newSingleThreadExecutor)
        hits (atom 0)]
    (try
      (.createContext server "/"
        (reify HttpHandler
          (handle [_ exchange]
            (swap! hits inc)
            (try
              (let [method (keyword (str/lower-case (.getRequestMethod exchange)))
                    uri (.getRequestURI exchange)
                    headers (into {}
                                  (for [[k v] (.entrySet (.getRequestHeaders exchange))]
                                    [(str/lower-case k) (first v)]))
                    response (checked-ring-handler
                              {:request-method method
                               :uri (.getPath uri)
                               :headers headers
                               :biff.xtdb/node ((model 'database))})
                    bytes (.getBytes (str (:body response))
                                     StandardCharsets/UTF_8)]
                (.sendResponseHeaders exchange
                                      (int (or (:status response) 500))
                                      (alength bytes))
                (with-open [out (.getResponseBody exchange)]
                  (.write out bytes)))
              (catch Throwable _
                (try (.sendResponseHeaders exchange 500 (long -1))
                     (catch Throwable _ nil)))
              (finally (.close exchange))))))
      (.setExecutor server executor)
      (.start server)
      {:server server :executor executor
       :port (.getPort (.getAddress server)) :hits hits}
      (catch Throwable failure
        (try (.stop server 0) (catch Throwable _ nil))
        (.shutdownNow executor)
        (throw failure)))))

(defn- replace-actual-listener-at-same-port! [scenario system]
  (let [original @(:server-state scenario)
        {:keys [server port]} original]
    (when-not (and (some? server) (integer? port)
                   (fn? (:biff.ring/handler system)))
      (throw (ex-info "Original listener fixture missing"
                      {:reason :missing-original-http-listener})))
    ;; Intentionally bypass managed stop; its historical adapter report
    ;; remains :verified-selected-adapter-start until managed shutdown.
    ((get-in scenario [:raw :biff.core/stop]) system)
    (let [replacement
          (bind-replacement-on-port! port (:biff.ring/handler system))]
      (reset! (:server-state scenario) replacement)
      {:old original :new replacement})))

(deftest real-same-port-server-replacement-invalidates-trusted-generation
  (let [s (scenario :honest)
        issued (config/issue-configured-checked-http-application!
                (assoc (original-source s true)
                       :listener-instance!
                       (fn [_] (:server @(:server-state s)))))]
    (try
      (let [{:keys [system instance-generation]} ((:start-once! issued) {})
            inspect! (fn [] ((:inspect-generation! issued)
                             instance-generation system))
            old-server (:server @(:server-state s))
            old-port (:port @(:server-state s))]
        (is (= :matches-original-host-reported-object
               (:physical-listener-instance (inspect!))))
        (let [{:keys [new]} (replace-actual-listener-at-same-port! s system)]
          (is (= old-port (:port new)))
          (is (not (identical? old-server (:server new))))
          (is (not (.isShutdown (:executor new))))
          (is (= :verified-configured-start
                 (:status ((:summary! issued)))))
          (is (= :configured-physical-listener-not-current
                 (reason inspect!)))
          (is (= :configured-physical-listener-not-current
                 (reason #((:inspect-running! issued) system))))
          (is (zero? @(:hits new))
              "Exact-object mismatch rejected before any liveness request"))
        (is (= :stopped (:status ((:stop! issued)))))
        (is (= :configured-instance-generation-not-current
               (reason inspect!))))
      (finally (cleanup! s issued)))))

(deftest real-same-port-replacement-exposes-uninstrumented-identity-limit
  ;; Without :listener-instance!, the second physical server can answer the
  ;; original wrapper's fresh one-use HTTP challenge on the original port.
  (let [s (scenario :honest)
        issued (config/issue-configured-checked-http-application!
                (original-source s true))]
    (try
      (let [{:keys [system instance-generation]} ((:start-once! issued) {})
            inspect! (fn [] ((:inspect-generation! issued)
                             instance-generation system))
            old-server (:server @(:server-state s))
            old-port (:port @(:server-state s))]
        (is (= :observed-at-current-inspection
               (:fresh-local-http-response (inspect!))))
        (let [{:keys [new]} (replace-actual-listener-at-same-port! s system)
              before @(:hits new)
              observed (inspect!)]
          (is (= old-port (:port new)))
          (is (not (identical? old-server (:server new))))
          (is (< before @(:hits new))
              "A real fresh challenge reached the replacement HttpServer")
          (is (= :observed-at-current-inspection
                 (:fresh-local-http-response observed)))
          (is (= :matches-startup-verified-loopback-port
                 (:original-local-listener-endpoint observed)))
          (is (= :not-established (:physical-listener-instance observed)))
          (is (= :not-established (:physical-instance-provenance observed))))
        (is (= :stopped (:status ((:stop! issued)))))
        (is (= :configured-instance-generation-not-current
               (reason inspect!))))
      (finally (cleanup! s issued)))))

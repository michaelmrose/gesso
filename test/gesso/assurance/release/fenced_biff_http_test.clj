(ns gesso.assurance.release.fenced-biff-http-test
  "v2269: Permanent original-Biff/XTDB socket tests of the host-local Ring
   traffic fence. Source, live listener, guard, preactivation denial, and
   fail-closed cleanup are exercised together. No release authority,
   asynchronous/SSE qualification or exclusive public ingress is claimed."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [gesso.assurance.release.fenced-biff-http :as fenced]
            [gesso.assurance.proof.checked-biff-http-application-configuration-fixture :as fixture])
  (:import [java.net URI]
           [java.net.http HttpClient HttpRequest HttpRequest$BodyPublishers
            HttpResponse$BodyHandlers]
           [java.time Duration]))

(use-fixtures :once fixture/with-real-application-fixture)

(defn- scenario [] (fixture/scenario :honest))
(defn- config [s]
  (assoc (fixture/original-source s true)
         :listener-instance! (fn [_] (:server @(:server-state s)))))
(defn- issue! [s guard]
  (fenced/issue-fenced-checked-biff-http!
   (constantly (get-in s [:env :catalog-source :catalog]))
   (config s) guard))
(defn- request [s method path]
  (let [port (:port @(:server-state s))
        req (-> (HttpRequest/newBuilder
                 (URI/create (str "http://127.0.0.1:" port path)))
                (.timeout (Duration/ofSeconds 10))
                (.method method (HttpRequest$BodyPublishers/noBody))
                (.build))]
    (.send (HttpClient/newHttpClient) req (HttpResponse$BodyHandlers/ofString))))
(defn- reason [f]
  (try (f) :no-error
       (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))
(defn- clean [s wrapper]
  (when wrapper (try ((:stop! wrapper)) (catch Throwable _ nil)))
  (when-let [stop (get-in s [:raw :biff.core/stop])]
    (try (stop {}) (catch Throwable _ nil))))

(deftest real-checked-biff-serves-host-local-fence
  (let [s (scenario)
        seen (atom [])
        wrapper (issue! s (fn [req] (swap! seen conj [(:request-method req) (:uri req)])
                            (not= :post (:request-method req))))]
    (try
      (is (= :issued (:status ((:summary! wrapper)))))
      (let [started ((:start-once! wrapper) {})]
        (is (= fenced/report-type (:type started)))
        (is (= :one-original-biff-listener-host-locally-fenced (:status started)))
        (is (= :observed-on-original-listener (:physical-inactive-fence started)))
        (is (= :none (:release-authority started)))
        (is (= :not-issued (:activation-eligibility started)))
        (is (= :not-issued (:completion-claim started)))
        (is (= :not-issued (:installation-eligibility started)))
        (is (= :running (:status ((:summary! wrapper)))))
        (is (= :active (get-in ((:summary! wrapper)) [:traffic-fence :phase])))
        (is (= :not-issued (:activation-eligibility ((:summary! wrapper)))))
        (is (= :none (:release-authority ((:summary! wrapper)))))
        (let [blocked (request s "POST" "/operations/request/claim")]
          (is (= 503 (.statusCode blocked)))
          (is (= "Protected application traffic is unavailable" (.body blocked))))
        (is (= 404 (.statusCode (request s "GET" "/not-a-route"))))
        (is (some #{[:post "/operations/request/claim"]} @seen))
        (is (= :fenced-biff-start-not-once-only
               (reason #((:start-once! wrapper) {})))))
      (finally (clean s wrapper)))))

(deftest stopped-fence-is-permanently-closed
  (let [s (scenario) wrapper (issue! s (constantly true))]
    (try
      ((:start-once! wrapper) {})
      (is (= 404 (.statusCode (request s "GET" "/not-a-route"))))
      (is (= :stopped (:status ((:stop! wrapper)))))
      (is (= :closed (get-in ((:summary! wrapper)) [:traffic-fence :phase])))
      (is (= :none (:release-authority ((:summary! wrapper)))))
      (is (= :not-issued (:activation-eligibility ((:summary! wrapper)))))
      (is (= :stopped (:status ((:summary! wrapper)))))
      (is (= :fenced-biff-start-not-once-only
             (reason #((:start-once! wrapper) {}))))
      (finally (clean s wrapper)))))

(deftest non-consumption-adapter-does-not-activate-the-fence
  (let [s (fixture/scenario :ignored)
        wrapper (issue! s (constantly true))]
    (try
      (is (not= :no-error (reason #((:start-once! wrapper) {}))))
      (is (not= :running (:status ((:summary! wrapper)))))
      (is (not= :active (get-in ((:summary! wrapper)) [:traffic-fence :phase])))
      (is (= :fenced-biff-start-not-once-only
             (reason #((:start-once! wrapper) {}))))
      (finally (clean s wrapper)))))

(deftest invalid-issuer-input-rejected-before-start
  (let [s (scenario)
        args [(fn [] (fenced/issue-fenced-checked-biff-http!
                       nil (config s) (constantly true)))
              (fn [] (fenced/issue-fenced-checked-biff-http!
                       (constantly {}) (dissoc (config s) :listener-instance!)
                       (constantly true)))
              (fn [] (fenced/issue-fenced-checked-biff-http!
                       (constantly {}) (config s) nil))]]
    (doseq [f args]
      (is (= :invalid-original-fenced-biff-input (reason f))))))

(deftest a-live-guard-revocation-denies-the-real-socket
  (let [s (scenario)
        allowed? (atom true)
        wrapper (issue! s (fn [_] @allowed?))]
    (try
      ((:start-once! wrapper) {})
      (is (= 404 (.statusCode (request s "GET" "/not-a-route"))))
      (reset! allowed? false)
      (is (= 503 (.statusCode (request s "GET" "/not-a-route"))))
      (is (= "Protected application traffic is unavailable"
             (.body (request s "GET" "/not-a-route"))))
      (reset! allowed? true)
      (is (= 404 (.statusCode (request s "GET" "/not-a-route"))))
      (finally (clean s wrapper)))))

(deftest preactivation-probe-exception-does-not-open-business-routing
  (let [candidate #'gesso.assurance.release.fenced-biff-http/original-private-probe?
        uuid "12345678-1234-1234-1234-123456789abc"
        path (str "/__gesso_assurance_http_consumption/" uuid)
        r {:request-method :get :uri path :headers {}}]
    (is (false? (candidate r)))
    (is (false? (candidate (assoc r :uri "/business/operation"
                                     :headers {"x-gesso-consumption-probe" "forged"}))))
    (is (false? (candidate (assoc r :request-method :post
                                     :headers {"x-gesso-consumption-probe" "forged"}))))
    (is (true? (candidate (assoc r :headers
                                      {"x-gesso-consumption-probe" "unverified"}))))
    ;; These two headers are always consumed by the checked original Biff
    ;; wrapper, so they never forward a request to business code.
    (is (true? (candidate (assoc r :uri "/business/operation"
                                      :headers {"x-gesso-local-liveness-probe" "wrong"}))))
    (is (true? (candidate (assoc r :uri "/business/operation"
                                      :headers {"x-gesso-operation-route-probe" "wrong"}))))))


(deftest malformed-selected-adapter-fails-before-any-listener-start
  (let [s (scenario)
        catalog (constantly (get-in s [:env :catalog-source :catalog]))
        conf (config s)
        selected (:adapter-module conf)]
    (try
      (doseq [raw [nil {} :invalid
                   (assoc selected :biff.core/id :unqualified)
                   (assoc selected :biff.core/start false)
                   (assoc selected :biff.core/stop false)
                   (assoc selected :surprise/key true)
                   (with-meta selected {:source :forged})]]
        (is (= :invalid-original-selected-adapter
               (reason #(fenced/issue-fenced-checked-biff-http!
                          catalog (assoc conf :adapter-module raw)
                          (constantly true))))))
      (is (nil? @(:server-state s)))
      (finally (clean s nil)))))

(deftest source-drift-before-start-blocks-and-consumes-one-shot-start
  (let [s (scenario)
        original (get-in s [:env :catalog-source :catalog])
        current (atom original)
        wrapper (fenced/issue-fenced-checked-biff-http!
                 (fn [] @current) (config s) (constantly true))]
    (try
      (swap! current update :invariants dissoc (first (keys (:invariants original))))
      (is (not= :no-error (reason #((:start-once! wrapper) {}))))
      (is (not= :running (:status ((:summary! wrapper)))))
      (is (not= :active (get-in ((:summary! wrapper)) [:traffic-fence :phase])))
      (is (= :none (:release-authority ((:summary! wrapper)))))
      (reset! current original)
      (is (= :fenced-biff-start-not-once-only
             (reason #((:start-once! wrapper) {}))))
      (finally (clean s wrapper)))))

(deftest guard-exceptions-and-nontrue-results-deny-live-http
  (let [s (scenario)
        guard-status (atom :permit)
        observed (atom 0)
        wrapper (issue! s
                        (fn [_]
                          (swap! observed inc)
                          (case @guard-status
                            :permit true
                            :refuse false
                            :nontrue :yes
                            :throw (throw (IllegalArgumentException. "guard unavailable")))))]
    (try
      ((:start-once! wrapper) {})
      (let [n @observed]
        (is (= 404 (.statusCode (request s "GET" "/not-a-route"))))
        (is (> @observed n)))
      (doseq [v [:refuse :nontrue :throw]]
        (reset! guard-status v)
        (let [before @observed
              http (request s "GET" "/not-a-route")]
          (is (= 503 (.statusCode http)) (str v))
          (is (= "Protected application traffic is unavailable" (.body http)))
          (is (> @observed before))))
      (reset! guard-status :permit)
      (is (= 404 (.statusCode (request s "GET" "/not-a-route"))))
      (finally (clean s wrapper)))))

(deftest private-probe-bypass-is-narrow-and-not-an-application-credential
  (let [probe #'gesso.assurance.release.fenced-biff-http/original-private-probe?
        uuid "abcdef12-1234-1234-1234-abcdef123456"
        original {:request-method :get
                  :uri (str "/__gesso_assurance_http_consumption/" uuid)
                  :headers {"x-gesso-consumption-probe" "original"}}]
    (is (true? (probe original)))
    (doseq [mutated [(dissoc original :headers)
                     (assoc original :headers {})
                     (assoc original :request-method :post)
                     (assoc original :uri "/operations/request/claim")
                     (assoc original :uri "/__gesso_assurance_http_consumption/wrong")
                     (assoc original :uri
                            "/__gesso_assurance_http_consumption/abcdef12-1234-1234-1234-abcdef123456/extra")
                     (assoc original :uri "/__gesso_release_traffic_fence/probe")]]
      (is (false? (probe mutated)) (pr-str mutated)))
    ;; These two headers are consumed by the original final checked wrapper,
    ;; even if the supplied nonce is incorrect. This test does not claim that
    ;; the reserved URL namespaces have been certified against the whole app.
    (doseq [h ["x-gesso-local-liveness-probe" "x-gesso-operation-route-probe"]]
      (is (true? (probe {:request-method :get :uri "/arbitrary"
                         :headers {h "not-verified"}}))))))

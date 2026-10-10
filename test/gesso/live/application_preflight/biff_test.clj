(ns gesso.live.application-preflight.biff-test
  "Direct contract tests for the internal Biff lifecycle adapter.

   These exercise the adapter with deliberately injected assembly recognition
   and render validation. They establish adapter behavior independently of
   expensive whole-application construction; the application-preflight tests
   remain responsible for proving that the public entry points supply their
   real current-assembly and rendered-surface functions.

   The stubbed Biff start calls check delegation and rejection order, not an
   actual deployment or the semantics of external server modules."
  (:require
   [clojure.test :refer [deftest is testing]]
   [com.biffweb.core :as biff]
   [gesso.http :as http]
   [gesso.live.application-preflight.biff :as lifecycle]))

(def ^:private assembly (Object.))
(def ^:private other-assembly (Object.))

(defn- current-assembly?
  [candidate]
  (identical? assembly candidate))

(defn- render-validator!
  [_assembly _surface _hiccup]
  true)

(defn- var-handler
  [_request]
  (http/html-response [:p "original"]))

(defn- ordinary-handler
  [_request]
  (http/html-response [:p "ok"]))

(def ^:private canonical-module
  (lifecycle/make-application-handler-module
   #'current-assembly?
   #'render-validator!))

(def ^:private test-modules
  [canonical-module])

(defn- exception-data
  [thunk]
  (try
    (thunk)
    nil
    (catch clojure.lang.ExceptionInfo e
      (ex-data e))))

(defn- error-kind
  [thunk]
  (:error/kind (exception-data thunk)))

(defn- installed
  [system]
  (lifecycle/install-application-handler
   current-assembly?
   render-validator!
   system))

(defn- module-start
  [system]
  ((:biff.core/start canonical-module) system))

(defn- start!
  ([modules-var order]
   (lifecycle/start-biff-application!
    current-assembly? canonical-module assembly
    {:fixture/initial :preserved} modules-var order))
  ([requested-assembly system modules-var order]
   (lifecycle/start-biff-application!
    current-assembly? canonical-module requested-assembly
    system modules-var order)))

(deftest canonical-biff-identifiers-remain-stable
  (is (= :gesso.live.application-preflight/html-response
         lifecycle/canonical-html-response-surface))
  (is (= :biff.ring/handler lifecycle/biff-ring-handler-key))
  (is (= :gesso.live.application-preflight/application-assembly
         lifecycle/application-assembly-system-key))
  (is (= :gesso.live.application-preflight/use-application-handler
         lifecycle/application-handler-module-id)))

(deftest wrapper-rejects-absent-and-spoofed-assemblies
  (doseq [bad [nil other-assembly
               {:gesso.live.application-preflight/type
                :gesso.live.application-preflight/application-assembly}]]
    (is (= :invalid-application-handler-assembly
           (error-kind
            #(lifecycle/wrap-application-handler
              current-assembly? render-validator! bad ordinary-handler))))))

(deftest wrapper-rejects-non-functions
  (doseq [bad [nil :keyword {} [] 42 "handler"]]
    (is (= :invalid-application-handler
           (error-kind
            #(lifecycle/wrap-application-handler
              current-assembly? render-validator! assembly bad))))))

(deftest wrapper-preserves-real-ring-response-and-guards-exact-hiccup
  (let [observed (atom [])
        body [:article {:id "one"} [:p "safe"]]
        handler (fn [req]
                  (is (= {:uri "/one"} req))
                  (http/html-response body))
        guarded (lifecycle/wrap-application-handler
                 current-assembly?
                 (fn [a surface hiccup]
                   (swap! observed conj [a surface hiccup])
                   true)
                 assembly handler)
        response (guarded {:uri "/one"})]
    (is (= 200 (:status response)))
    (is (= http/html-content-type
           (get-in response [:headers "content-type"])))
    (is (re-find #"safe" (:body response)))
    (is (= [[assembly lifecycle/canonical-html-response-surface body]]
           @observed))))

(deftest wrapper-checks-every-html-response-not-just-construction
  (let [calls (atom [])
        guarded (lifecycle/wrap-application-handler
                 current-assembly?
                 (fn [_ _ hiccup]
                   (swap! calls conj hiccup)
                   true)
                 assembly
                 (fn [{:keys [n]}]
                   (http/html-response [:p (str n)])))]
    (is (= 200 (:status (guarded {:n 1}))))
    (is (= 200 (:status (guarded {:n 2}))))
    (is (= [[:p "1"] [:p "2"]] @calls))))

(deftest wrapper-propagates-rejection-before-html-serialization
  (let [seen (atom [])
        expected (ex-info "not admitted" {:error/kind :fixture/rejected})
        guarded (lifecycle/wrap-application-handler
                 current-assembly?
                 (fn [_ _ hiccup]
                   (swap! seen conj hiccup)
                   (throw expected))
                 assembly
                 (fn [_] (http/html-response [:div "forbidden"])))]
    (is (identical? expected
                    (try
                      (guarded {})
                      (catch clojure.lang.ExceptionInfo e e))))
    (is (= [[:div "forbidden"]] @seen))))

(deftest wrapper-retains-existing-outer-guard
  (let [events (atom [])
        guarded (lifecycle/wrap-application-handler
                 current-assembly?
                 (fn [_ _ _] (swap! events conj :inner) true)
                 assembly ordinary-handler)]
    (http/with-html-response-preflight
     (fn [_] (swap! events conj :outer) true)
     #(guarded {}))
    (is (= [:outer :inner] @events))))

(deftest wrapper-does-not-claim-to-guard-raw-ring-responses
  (let [calls (atom 0)
        raw {:status 200
             :headers {"content-type" "text/html"}
             :body "<p>raw</p>"}
        guarded (lifecycle/wrap-application-handler
                 current-assembly?
                 (fn [& _] (swap! calls inc) true)
                 assembly (constantly raw))]
    (is (= raw (guarded {})))
    ;; This escape boundary is an explicit documented limitation.
    (is (zero? @calls))))

(deftest wrapper-accepts-handler-var-and-resolves-it-at-call-time
  (let [guarded (lifecycle/wrap-application-handler
                 current-assembly? render-validator!
                 assembly #'var-handler)]
    (is (re-find #"original" (:body (guarded {}))))
    (with-redefs [var-handler
                  (fn [_] (http/html-response [:p "rebound"]))]
      (is (re-find #"rebound" (:body (guarded {})))))))

(deftest installer-requires-an-actual-system-map
  (doseq [bad [nil [] :system "system" 3]]
    (is (= :invalid-application-handler-system
           (error-kind #(installed bad))))))

(deftest installer-requires-current-authoritative-assembly
  (doseq [candidate [nil other-assembly {:type :pretend}]]
    (is (= :invalid-application-handler-assembly
           (error-kind
            #(installed {lifecycle/application-assembly-system-key candidate
                         :biff.ring/handler ordinary-handler}))))))

(deftest installer-requires-present-and-callable-ring-handler
  (is (= :missing-biff-ring-handler
         (error-kind
          #(installed {lifecycle/application-assembly-system-key assembly}))))
  (doseq [bad [nil {} :handler 42 []]]
    (is (= :invalid-biff-ring-handler
           (error-kind
            #(installed {lifecycle/application-assembly-system-key assembly
                         :biff.ring/handler bad}))))))

(deftest installer-preserves-unrelated-system-data-and-removes-temporary-assembly
  (let [before {lifecycle/application-assembly-system-key assembly
                :biff.ring/handler ordinary-handler
                :fixture/preserved {:nested [1 2]}
                :fixture/other true}
        after (installed before)]
    (is (not (contains? after lifecycle/application-assembly-system-key)))
    (is (= (dissoc before
                   lifecycle/application-assembly-system-key
                   :biff.ring/handler)
           (dissoc after :biff.ring/handler)))
    (is (fn? (:biff.ring/handler after)))
    (is (not (identical? ordinary-handler (:biff.ring/handler after))))
    (is (= 200 (:status ((:biff.ring/handler after) {}))))))

(deftest installer-accepts-a-ring-handler-var
  (let [result (installed
                {lifecycle/application-assembly-system-key assembly
                 :biff.ring/handler #'var-handler})]
    (is (re-find #"original"
                 (:body ((:biff.ring/handler result) {}))))))

(deftest biff-module-has-one-native-lifecycle-start-and-canonical-id
  (is (= lifecycle/application-handler-module-id
         (:biff.core/id canonical-module)))
  (is (fn? (:biff.core/start canonical-module)))
  (is (not (contains? canonical-module :biff.core/init)))
  (is (not (contains? canonical-module :biff.core/stop)))
  (let [after (module-start
               {lifecycle/application-assembly-system-key assembly
                :biff.ring/handler ordinary-handler})]
    (is (fn? (:biff.ring/handler after)))
    (is (not (contains? after lifecycle/application-assembly-system-key)))))

(deftest module-retains-validator-vars-and-observes-rebinding
  (let [observed (atom [])
        module (lifecycle/make-application-handler-module
                #'current-assembly? #'render-validator!)
        handler (:biff.ring/handler
                 ((:biff.core/start module)
                  {lifecycle/application-assembly-system-key assembly
                   :biff.ring/handler ordinary-handler}))]
    (with-redefs [render-validator!
                  (fn [a surface hiccup]
                    (swap! observed conj [a surface hiccup])
                    true)]
      (is (= 200 (:status (handler {})))))
    (is (= [[assembly lifecycle/canonical-html-response-surface [:p "ok"]]]
           @observed))))

(deftest module-retains-current-assembly-var-and-observes-rebinding
  (let [module (lifecycle/make-application-handler-module
                #'current-assembly? #'render-validator!)]
    (with-redefs [current-assembly? (constantly false)]
      (is (= :invalid-application-handler-assembly
             (error-kind
              #((:biff.core/start module)
                {lifecycle/application-assembly-system-key assembly
                 :biff.ring/handler ordinary-handler})))))))

(deftest startup-rejects-stale-assembly-before-delegating
  (let [calls (atom [])]
    (with-redefs [biff/start (fn [& args] (swap! calls conj args))]
      (is (= :invalid-biff-application-start-assembly
             (error-kind
              #(start! other-assembly {} #'test-modules
                       [lifecycle/application-handler-module-id]))))
      (is (= [] @calls)))))

(deftest startup-rejects-invalid-initial-system-before-delegating
  (let [calls (atom 0)]
    (with-redefs [biff/start (fn [& _] (swap! calls inc))]
      (doseq [bad [nil [] :not-a-map]]
        (is (= :invalid-biff-application-initial-system
               (error-kind
                #(start! assembly bad #'test-modules
                         [lifecycle/application-handler-module-id])))))
      (is (zero? @calls)))))

(deftest startup-requires-modules-as-a-var
  (let [calls (atom 0)]
    (with-redefs [biff/start (fn [& _] (swap! calls inc))]
      (doseq [bad [nil [] test-modules :modules]]
        (is (= :invalid-biff-application-modules-var
               (error-kind
                #(start! bad [lifecycle/application-handler-module-id])))))
      (is (zero? @calls)))))

(deftest startup-requires-exactly-one-handler-module
  (with-redefs [biff/start (fn [& _] (throw (ex-info "must not start" {})))]
    (doseq [candidates [[] [canonical-module canonical-module]
                        [(assoc canonical-module :biff.core/id :fixture/other)]]]
      (with-redefs [test-modules candidates]
        (is (= :missing-or-duplicate-application-handler-module
               (error-kind
                #(start! #'test-modules
                         [lifecycle/application-handler-module-id]))))))))

(deftest startup-rejects-forged-module-with-canonical-id
  (with-redefs [test-modules
                [{:biff.core/id lifecycle/application-handler-module-id
                  :biff.core/start identity}]
                biff/start (fn [& _] (throw (ex-info "must not start" {})))]
    (is (= :invalid-application-handler-module
           (error-kind
            #(start! #'test-modules
                     [lifecycle/application-handler-module-id]))))))

(deftest startup-rejects-nonsequential-or-wrong-first-order
  (with-redefs [biff/start (fn [& _] (throw (ex-info "must not start" {})))]
    (doseq [order [nil {} #{lifecycle/application-handler-module-id}
                   :invalid-order]]
      (is (= :invalid-biff-application-start-order
             (error-kind #(start! #'test-modules order)))))
    (doseq [order [[] [:fixture/other lifecycle/application-handler-module-id]]]
      (is (= :application-handler-module-not-first
             (error-kind #(start! #'test-modules order)))))))

(deftest startup-passes-exact-module-var-order-and-authoritative-assembly-to-biff
  (let [seen (atom nil)
        order (list lifecycle/application-handler-module-id :fixture/later)
        supplied {:fixture/initial :present
                  lifecycle/application-assembly-system-key other-assembly}]
    (with-redefs [biff/start
                  (fn [system modules-var start-order]
                    (reset! seen [system modules-var start-order])
                    :started)]
      (is (= :started (start! assembly supplied #'test-modules order))))
    (let [[system modules-var start-order] @seen]
      (is (= :present (:fixture/initial system)))
      (is (identical? assembly
                      (get system lifecycle/application-assembly-system-key)))
      (is (identical? #'test-modules modules-var))
      (is (identical? order start-order)))))

(deftest startup-short-arity-defaults-to-empty-initial-system
  (let [calls (atom nil)]
    (with-redefs [biff/start
                  (fn [system modules-var start-order]
                    (reset! calls [system modules-var start-order])
                    :short-started)]
      (is (= :short-started
             (lifecycle/start-biff-application!
              current-assembly?
              canonical-module
              assembly
              #'test-modules
              [lifecycle/application-handler-module-id]))))
    (let [[system modules-var order] @calls]
      (is (= #{lifecycle/application-assembly-system-key}
             (set (keys system))))
      (is (identical? assembly
                      (get system lifecycle/application-assembly-system-key)))
      (is (identical? #'test-modules modules-var))
      (is (= [lifecycle/application-handler-module-id] order)))))

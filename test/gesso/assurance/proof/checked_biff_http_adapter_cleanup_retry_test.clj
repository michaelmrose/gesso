(ns gesso.assurance.proof.checked-biff-http-adapter-cleanup-retry-test
  "v2288: Permanent original guarded Biff adapter retry regression.
   A selected host's explicit idempotence promise permits one physical stop
   callback retry, but a returning callback is NOT listener-closure proof.
   All invocations retain the original Biff system selected on first stop."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [gesso.assurance.proof.checked-biff-http-adapter-start :as guarded]
            [gesso.assurance.proof.checked-biff-http-application-configuration-fixture
             :as fixture :refer [scenario reason with-real-application-fixture]]
            [gesso.assurance.proof.checked-biff-http-adapter-start-test :as original]
            [gesso.live.application-preflight :as app]))

(use-fixtures :once with-real-application-fixture)

(defn- issue [s stop-callback policy]
  (let [raw (assoc (:raw s) :biff.core/stop stop-callback)
        origin (fn [_] (str "http://127.0.0.1:"
                            (:port @(:server-state s))))
        issued (guarded/issue-checked-http-adapter-start!
                (:probe s) raw origin policy)]
    {:issued issued
     :scenario (assoc s :guarded issued
                      :modules [app/application-handler-module
                                (:module (:adopted s))
                                (:module (:probe s))
                                (:module issued)])}))

(defn- start! [s]
  ;; Reuse the original, real Biff/XTDB integration fixture and module order.
  (let [f (var-get (ns-resolve
                    'gesso.assurance.proof.checked-biff-http-adapter-start-test
                    'start!))]
    (f s)))

(defn- stopped? [s]
  (boolean (and @(:server-state s)
                (.isShutdown ^java.util.concurrent.ExecutorService
                             (:executor @(:server-state s))))))

(defn- raw-stop! [s]
  (when-let [stop (get-in s [:raw :biff.core/stop])]
    (try (stop {}) (catch Throwable _ nil))))

(defn- policy [] {:idempotent-stop-retry? true})

(deftest retry-policy-requires-an-exact-unadorned-opt-in
  (let [s (scenario :honest)
        valid (issue s (:biff.core/stop (:raw s)) (policy))]
    (is (= :explicit-host-idempotence
           (:stop-retry-policy ((:summary! (:issued valid))))))
    (is (= :not-attempted
           (:selected-adapter-cleanup ((:summary! (:issued valid))))))
    (doseq [bad [nil true false {:idempotent-stop-retry? false}
                 {:idempotent-stop-retry? nil}
                 {:idempotent-stop-retry? true :unexpected true}
                 (with-meta (policy) {:forged true})]]
      (is (= :invalid-cleanup-retry-policy
             (reason #(issue s (:biff.core/stop (:raw s)) bad)))
          (str "Reject invalid retry declaration " (pr-str bad))))
    (is (nil? @(:server-state s))
        "Unstarted retry declarations must not open any physical listener")))

(deftest default-three-argument-issuer-remains-one-shot
  (let [s (scenario :honest)
        original-stop (:biff.core/stop (:raw s))
        calls (atom 0)
        adapter (assoc (:raw s) :biff.core/stop
                       (fn [_]
                         (swap! calls inc)
                         (throw (ex-info "Injected first-stop failure" {}))))
        issued (guarded/issue-checked-http-adapter-start!
                (:probe s) adapter
                (fn [_] (str "http://127.0.0.1:"
                             (:port @(:server-state s)))))
        cfg (assoc s :guarded issued
                   :modules [app/application-handler-module
                             (:module (:adopted s)) (:module (:probe s))
                             (:module issued)])]
    (try
      (let [system (start! cfg)]
        ((:biff.core/stop (:module issued)) system)
        ((:biff.core/stop (:module issued)) {:foreign :system})
        (is (= 1 @calls))
        (is (= :stop-cleanup-not-established
               (:status ((:summary! issued)))))
        (is (= :unissued-cleanup-retry (reason (:retry-cleanup! issued))))
        (is (= :not-enabled (:stop-retry-policy ((:summary! issued)))))
        (is (not (stopped? s))
            "Unconfirmed first-stop failure cannot be called closed"))
      (finally (try (original-stop {}) (catch Throwable _ nil))))))

(deftest physical-retry-keeps-exact-original-stop-target-and-only-two-attempts
  (let [s (scenario :honest)
        raw (:biff.core/stop (:raw s))
        attempts (atom [])
        {:keys [issued scenario]}
        (issue s (fn [system]
                   (swap! attempts conj system)
                   (when (= 1 (count @attempts))
                     (throw (ex-info "First stop fails before closure" {})))
                   (raw system))
               (policy))]
    (try
      (is (= :original-stop-not-attempted
             (reason (:retry-cleanup! issued))))
      (let [system (start! scenario)]
        (is (= :verified-selected-adapter-start
               (:status ((:summary! issued)))))
        ((:biff.core/stop (:module issued)) system)
        (is (= 1 (count @attempts)))
        (is (map? (first @attempts))
            "The guarded adapter captured a concrete stop-system map")
        (is (= :stop-cleanup-not-established
               (:status ((:summary! issued)))))
        (is (not (stopped? s)))
        ((:biff.core/stop (:module issued)) {:forged :replacement})
        (is (= 1 (count @attempts))
            "Additional Biff stop invocations must not switch the target")
        (let [result ((:retry-cleanup! issued))]
          (is (= :callback-returned (:status result)))
          (is (= 2 (:attempts result)))
          (is (= :original-selected-stop-callback-only (:scope result)))
          (is (= :not-established (:physical-listener-closure result)))
          (is (= :none (:release-authority result)))
          (is (= :not-issued (:activation-eligibility result))))
        (is (= 2 (count @attempts)))
        (is (identical? (first @attempts) (second @attempts))
            "Both original stop callbacks must receive the SAME captured system")
        (is (stopped? s))
        (is (= :stopped (:status ((:summary! issued)))))
        (is (= :completed (:selected-adapter-cleanup ((:summary! issued)))))
        (is (= 2 (:original-stop-attempts ((:summary! issued)))))
        (is (= :callback-returned (:status ((:retry-cleanup! issued)))))
        (is (= 2 (count @attempts))
            "After success, repeated observation must not call stop again"))
      (finally (raw-stop! s)))))

(deftest two-callback-failures-exhaust-the-retry-budget
  (let [s (scenario :honest)
        raw (:biff.core/stop (:raw s))
        attempts (atom 0)
        {:keys [issued scenario]}
        (issue s (fn [_]
                   (swap! attempts inc)
                   (throw (ex-info "Stop always fails" {})))
               (policy))]
    (try
      (let [system (start! scenario)]
        ((:biff.core/stop (:module issued)) system)
        (is (= :cleanup-unconfirmed (:status ((:retry-cleanup! issued)))))
        (is (= 2 @attempts))
        (is (= :stop-cleanup-not-established
               (:status ((:summary! issued)))))
        (is (= :original-stop-retry-budget-exhausted
               (reason (:retry-cleanup! issued))))
        ((:biff.core/stop (:module issued)) {:bogus :target})
        (is (= 2 @attempts))
        (is (not (stopped? s))))
      (finally (raw-stop! s)))))

(deftest a-returning-stop-callback-does-not-prove-physical-closure
  (let [s (scenario :honest)
        attempts (atom 0)
        {:keys [issued scenario]}
        (issue s (fn [_]
                   (if (= 1 (swap! attempts inc))
                     (throw (ex-info "First stop fails" {}))
                     :returned-without-closing-listener))
               (policy))]
    (try
      (let [system (start! scenario)]
        ((:biff.core/stop (:module issued)) system)
        (is (= :callback-returned (:status ((:retry-cleanup! issued)))))
        (is (= :stopped (:status ((:summary! issued)))))
        (is (= 2 @attempts))
        (is (not (stopped? s)))
        (is (= :not-established
               (:physical-listener-closure ((:retry-cleanup! issued))))))
      (finally (raw-stop! s)))))

(deftest reentrant-retry-cannot-use-the-in-progress-stop-attempt
  (let [s (scenario :honest)
        raw (:biff.core/stop (:raw s))
        attempt (atom 0)
        observed (atom [])
        current (atom nil)
        {:keys [issued scenario]}
        (issue s (fn [system]
                   (let [n (swap! attempt inc)]
                     (when (= n 1)
                       (throw (ex-info "First physical stop fails" {})))
                     (swap! observed conj (reason (:retry-cleanup! @current)))
                     (raw system)))
               (policy))]
    (reset! current issued)
    (try
      (let [system (start! scenario)]
        ((:biff.core/stop (:module issued)) system)
        (is (= :callback-returned (:status ((:retry-cleanup! issued)))))
        (is (= 2 @attempt))
        (is (= [:original-stop-in-progress] @observed))
        (is (= :stopped (:status ((:summary! issued)))))
        (is (stopped? s)))
      (finally (raw-stop! s)))))

(deftest exact-issued-identity-rejects-substitution-of-retry-callback
  (let [s (scenario :honest)
        {:keys [issued]} (issue s (:biff.core/stop (:raw s)) (policy))]
    (is (= :original-issued-guarded-http-adapter
           (:scope (guarded/require-original-issued-http-adapter-start! issued))))
    (is (= :unissued-or-substituted-guarded-adapter
           (reason #(guarded/require-original-issued-http-adapter-start!
                     (assoc issued :retry-cleanup! (fn [] :forged))))))
    (is (= :unissued-or-substituted-guarded-adapter
           (reason #(guarded/require-original-issued-http-adapter-start!
                     (assoc issued :summary! (fn [] :forged))))))
    (is (= :invalid-original-guarded-adapter-shape
           (reason #(guarded/require-original-issued-http-adapter-start!
                     (assoc issued :extra :credential)))))))

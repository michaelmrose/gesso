(ns gesso.live.browser.choreo-test
  "Browser Choreo binding tests.

   These tests deliberately treat gesso.live.browser.choreo as a physical
   integration layer over shell + adapter. Portable machine semantics are tested
   elsewhere; this namespace freezes the absence of an independent browser
   execution runtime and exercises the physical realization/callback boundary."
  (:require
   [cljs.test :refer-macros [async deftest is testing]]
   [gesso.choreo.core :as c]
   [gesso.choreo.machine :as machine]
   [gesso.choreo.project :as project]
   [gesso.live.browser.adapter :as adapter]
   [gesso.live.browser.choreo :as choreo]
   [gesso.live.browser.fx :as fx]
   [gesso.live.browser.shell :as shell]))

;; =============================================================================
;; Helpers / fixture choreographies
;; =============================================================================

(defn- thrown
  [f]
  (try
    (f)
    nil
    (catch :default error
      error)))

(defn- thrown-data
  [f]
  (some-> (thrown f) ex-data))

(defn- browser-execution
  [choreography]
  (machine/start
   (project/project choreography :browser)))

(defn- local-once
  ([]
   (local-once :browser/work))
  ([action-id]
   (c/->choreography
    {:initial :work
     :states
     {:work (c/local :browser action-id :done {:outputs #{:value}})
      :done (c/return :done)}})))

(defn- send-once
  []
  (c/->choreography
   {:initial :send
    :states
    {:send (c/communicate :browser :server :browser/message :done
                          {:open-payload? true})
     :done (c/return :done)}}))

(defn- await-once
  []
  (c/->choreography
   {:initial :wait
    :states
    {:wait (c/await :browser {:browser/continue :done})
     :done (c/return :done)}}))

(defn- receive-once
  []
  (c/->choreography
   {:initial :receive
    :states
    {:receive (c/communicate :server :browser :server/message :done
                             {:open-payload? true})
     :done (c/return :done)}}))

(defn- await-timeout
  []
  (c/->choreography
   {:initial :wait
    :states
    {:wait (c/await :browser {:browser/timeout :done})
     :done (c/return :done)}}))

(defn- invariant-clean?
  [runtime]
  (empty? (adapter/invariant-errors (choreo/state runtime))))

;; =============================================================================
;; Identity / construction / ownership
;; =============================================================================

(deftest runtime-identity-and-owned-effect-kinds-test
  (is (= 2 choreo/runtime-version))
  (is (= :gesso.live.browser.choreo/runtime choreo/runtime-type))
  (is (= :gesso.live.browser.choreo/execution-ref choreo/execution-ref-type))
  (is (= #{:machine/local :machine/send :transport/send}
         choreo/owned-effect-kinds)))

(deftest create-attaches-only-three-physical-effect-handlers-test
  (let [shell-runtime (shell/create)
        runtime (choreo/create shell-runtime)]
    (is (choreo/runtime? runtime))
    (is (identical? shell-runtime (choreo/shell-runtime runtime)))
    (is (= choreo/owned-effect-kinds
           (set (keys (shell/handlers shell-runtime)))))
    (is (= choreo/owned-effect-kinds
           (:attached-effect-kinds (choreo/diagnostics runtime))))
    (is (invariant-clean? runtime))))

(deftest runtime-owns-no-semantic-or-browser-resource-registries-test
  (let [runtime (choreo/create (shell/create))]
    (doseq [forbidden-key [:executions
                           :execution-metadata
                           :timers
                           :terminal-history
                           :targets
                           :continuity
                           :resources]]
      (is (not (contains? runtime forbidden-key))
          (str "browser.choreo must not own " forbidden-key)))
    (is (= #{} (choreo/active-execution-ids runtime)))))

(deftest create-rejects-unknown-options-before-mutating-shell-test
  (let [shell-runtime (shell/create)
        data (thrown-data
              #(choreo/create shell-runtime {:mystery true}))]
    (is (= :unknown-options (:error/kind data)))
    (is (= #{:mystery} (:unknown-keys data)))
    (is (empty? (shell/handlers shell-runtime)))))

(deftest create-validates-registration-maps-test
  (doseq [[options expected-kind]
          [[{:local-actions [:not :a-map]} :invalid-map]
           [{:fx-handlers [:not :a-map]} :invalid-map]
           [{:local-actions {"not-keyword" (fn [_] nil)}} :invalid-keyword]
           [{:local-actions {:browser/work 42}} :invalid-callable]
           [{:fx-handlers {:browser/fx 42}} :invalid-callable]
           [{:send-payload 42} :invalid-callable]
           [{:transport-send 42} :invalid-callable]]]
    (let [shell-runtime (shell/create)
          data (thrown-data #(choreo/create shell-runtime options))]
      (is (= expected-kind (:error/kind data)) (pr-str options))
      (is (empty? (shell/handlers shell-runtime)) (pr-str options)))))

(deftest create-refuses-effect-slot-collision-atomically-test
  (let [existing (fn [_] :existing)
        shell-runtime (shell/create {:handlers {:machine/send existing}})
        data (thrown-data #(choreo/create shell-runtime))]
    (is (= :effect-handler-collision (:error/kind data)))
    (is (= #{:machine/send} (:effect-kinds data)))
    (is (= {:machine/send existing}
           (shell/handlers shell-runtime)))))

;; =============================================================================
;; Physical registration APIs
;; =============================================================================

(deftest local-action-registration-is-per-runtime-test
  (let [runtime-a (choreo/create (shell/create))
        runtime-b (choreo/create (shell/create))
        handler (fn [_] {:value 1})]
    (is (= :browser/work
           (choreo/register-local-action! runtime-a :browser/work handler)))
    (is (identical? handler (:browser/work (choreo/local-actions runtime-a))))
    (is (empty? (choreo/local-actions runtime-b)))
    (is (= :browser/work
           (choreo/unregister-local-action! runtime-a :browser/work)))
    (is (empty? (choreo/local-actions runtime-a)))))

(deftest fx-handler-registration-is-per-runtime-test
  (let [runtime-a (choreo/create (shell/create))
        runtime-b (choreo/create (shell/create))
        handler (fn [_ value] (inc value))]
    (is (= :test/inc
           (choreo/register-fx-handler! runtime-a :test/inc handler)))
    (is (identical? handler (:test/inc (choreo/fx-handlers runtime-a))))
    (is (empty? (choreo/fx-handlers runtime-b)))
    (is (= :test/inc
           (choreo/unregister-fx-handler! runtime-a :test/inc)))
    (is (empty? (choreo/fx-handlers runtime-a)))))

(deftest send-and-transport-handler-setters-are-physical-configuration-only-test
  (let [runtime (choreo/create (shell/create))
        send (fn [_] {:payload true})
        transport (fn [_] :sent)]
    (is (true? (choreo/set-send-payload-handler! runtime send)))
    (is (identical? send (choreo/send-payload-handler runtime)))
    (is (true? (choreo/set-transport-handler! runtime transport)))
    (is (identical? transport (choreo/transport-handler runtime)))
    (is (true? (choreo/set-send-payload-handler! runtime nil)))
    (is (nil? (choreo/send-payload-handler runtime)))
    (is (true? (choreo/set-transport-handler! runtime nil)))
    (is (nil? (choreo/transport-handler runtime)))))

(deftest registration-ids-and-values-are-validated-test
  (let [runtime (choreo/create (shell/create))]
    (is (= :invalid-keyword
           (:error/kind
            (thrown-data #(choreo/register-local-action! runtime "bad" (fn [_] nil))))))
    (is (= :invalid-callable
           (:error/kind
            (thrown-data #(choreo/register-local-action! runtime :browser/work 42)))))
    (is (= :invalid-keyword
           (:error/kind
            (thrown-data #(choreo/register-fx-handler! runtime "bad" (fn [_] nil))))))
    (is (= :invalid-callable
           (:error/kind
            (thrown-data #(choreo/register-fx-handler! runtime :test/fx 42)))))
    (is (= :invalid-callable
           (:error/kind
            (thrown-data #(choreo/set-send-payload-handler! runtime 42)))))
    (is (= :invalid-callable
           (:error/kind
            (thrown-data #(choreo/set-transport-handler! runtime 42)))))))

;; =============================================================================
;; Local realization through shell + adapter
;; =============================================================================

(deftest local-action-goes-through-adapter-and-retires-on-completion-test
  (let [shell-runtime (shell/create)
        seen (atom nil)
        runtime (choreo/create
                 shell-runtime
                 {:local-actions
                  {:browser/work
                   (fn [ctx]
                     (reset! seen ctx)
                     {:value 42})}})
        result (choreo/start-execution!
                runtime :execution/local
                (browser-execution (local-once)))]
    (is (= :dispatched (:status result)))
    (is (nil? (:execution-ref result)))
    (is (nil? (choreo/execution runtime :execution/local)))
    (is (false? (choreo/active? runtime :execution/local)))
    (is (= :browser/work (get-in @seen [choreo/action-key :action])))
    (is (= :execution/local (get @seen choreo/execution-id-key)))
    (is (pos-int? (get @seen choreo/generation-key)))
    (is (pos-int? (get @seen choreo/effect-generation-key)))
    (is (invariant-clean? runtime))))

(deftest local-action-receives-inputs-and-current-fx-handler-map-test
  (let [seen (atom nil)
        runtime (choreo/create (shell/create))
        inc-handler (fn [_ value] (inc value))
        local-machine
        (fx/machine
         :test/local
         :start
         (fn [ctx]
           (reset! seen ctx)
           {:value [:test/inc 41]}))]
    (choreo/register-fx-handler! runtime :test/inc inc-handler)
    (choreo/register-local-action! runtime :browser/work local-machine)
    (choreo/start-execution! runtime :execution/fx
                             (browser-execution (local-once)))
    (is (identical? inc-handler
                    (get-in @seen [:biff.fx/handlers :test/inc])))
    (is (= :execution/fx (get @seen choreo/execution-id-key)))
    (is (false? (choreo/active? runtime :execution/fx)))
    (is (invariant-clean? runtime))))

(deftest missing-local-realization-fails-physical-effect-and-retires-test
  (let [errors (atom [])
        shell-runtime (shell/create {:on-error #(swap! errors conj %)})
        runtime (choreo/create shell-runtime)
        result (choreo/start-execution!
                runtime :execution/missing-local
                (browser-execution (local-once :browser/missing)))]
    (is (= :failed (first (:effect-results result))))
    (is (false? (choreo/active? runtime :execution/missing-local)))
    (is (seq @errors))
    (is (invariant-clean? runtime))))

(deftest rejected-local-promise-retires-through-shell-not-choreo-test
  (async done
    (let [errors (atom [])
          shell-runtime (shell/create {:on-error #(swap! errors conj %)})
          runtime (choreo/create
                   shell-runtime
                   {:local-actions
                    {:browser/work
                     (fn [_]
                       (js/Promise.reject (js/Error. "local failed")))}})
          result (choreo/start-execution!
                  runtime :execution/rejected-local
                  (browser-execution (local-once)))]
      (is (= :pending (first (:effect-results result))))
      (.then (js/Promise.resolve nil)
             (fn [_]
               (js/setTimeout
                (fn []
                  (is (false? (choreo/active? runtime :execution/rejected-local)))
                  (is (seq @errors))
                  (is (invariant-clean? runtime))
                  (done))
                0))))))

;; =============================================================================
;; Send payload and physical transport remain distinct
;; =============================================================================

(deftest send-payload-and-transport-are-distinct-physical-boundaries-test
  (let [payload-context (atom nil)
        transport-context (atom nil)
        runtime
        (choreo/create
         (shell/create)
         {:send-payload
          (fn [ctx]
            (reset! payload-context ctx)
            {:wire 7})
          :transport-send
          (fn [ctx]
            (reset! transport-context ctx)
            :sent)})
        result
        (choreo/start-execution!
         runtime :execution/send
         (browser-execution (send-once)))]
    (is (nil? (:execution-ref result)))
    (is (= :execution/send (get @payload-context choreo/execution-id-key)))
    (is (= :browser/message (get-in @payload-context [choreo/action-key :event])))
    (is (= {:wire 7}
           (get-in @transport-context [choreo/message-key :payload])))
    (is (= :browser/message
           (get-in @transport-context [choreo/message-key :event])))
    (is (not (contains? @payload-context choreo/message-key)))
    (is (not (contains? @transport-context choreo/action-key)))
    (is (false? (choreo/active? runtime :execution/send)))
    (is (invariant-clean? runtime))))

(deftest missing-send-payload-handler-retires-execution-test
  (let [runtime (choreo/create (shell/create)
                               {:transport-send (fn [_] :sent)})
        result (choreo/start-execution!
                runtime :execution/no-payload
                (browser-execution (send-once)))]
    (is (= :failed (first (:effect-results result))))
    (is (false? (choreo/active? runtime :execution/no-payload)))
    (is (invariant-clean? runtime))))

(deftest missing-transport-handler-leaves-send-boundary-retryable-test
  (let [transported (atom nil)
        runtime (choreo/create (shell/create)
                               {:send-payload (fn [_] {:wire 1})})
        result (choreo/start-execution!
                runtime :execution/no-transport
                (browser-execution (send-once)))
        ref (:execution-ref result)]
    (is (= :dispatched (:status result)))
    ;; Physical transport failure must not fabricate successful semantic send.
    (is (choreo/active? runtime :execution/no-transport))
    (is (choreo/execution-ref? ref))
    (choreo/set-transport-handler!
     runtime
     (fn [ctx]
       (reset! transported ctx)
       :sent))
    (choreo/retry! runtime ref)
    (is (= {:wire 1}
           (get-in @transported [choreo/message-key :payload])))
    (is (false? (choreo/active? runtime :execution/no-transport)))
    (is (invariant-clean? runtime))))

;; =============================================================================
;; Captured generation is mandatory for every external callback
;; =============================================================================

(deftest environment-resumption-uses-captured-generation-test
  (let [runtime (choreo/create (shell/create))
        start (choreo/start-execution!
               runtime :execution/environment
               (browser-execution (await-once)))
        execution-ref (:execution-ref start)]
    (is (choreo/execution-ref? execution-ref))
    (is (choreo/active? runtime :execution/environment))
    (choreo/deliver-environment!
     runtime execution-ref
     (machine/environment-event :browser :browser/continue))
    (is (false? (choreo/active? runtime :execution/environment)))
    (is (invariant-clean? runtime))))

(deftest stale-environment-callback-cannot-resume-replacement-generation-test
  (let [runtime (choreo/create (shell/create))
        first-start (choreo/start-execution!
                     runtime :execution/replaced
                     (browser-execution (await-once)))
        old-ref (:execution-ref first-start)
        _ (choreo/start-execution!
           runtime :execution/replaced
           (browser-execution (await-once))
           {:replace-execution? true})
        new-ref (choreo/execution-ref runtime :execution/replaced)
        envelope (machine/environment-event :browser :browser/continue)
        stale (choreo/deliver-environment! runtime old-ref envelope)]
    (is (not= (:generation old-ref) (:generation new-ref)))
    (is (= :stale-execution-generation
           (get-in stale [:effects 0 1 :reason])))
    (is (choreo/active? runtime :execution/replaced))
    (choreo/deliver-environment! runtime new-ref envelope)
    (is (false? (choreo/active? runtime :execution/replaced)))
    (is (invariant-clean? runtime))))

(deftest participant-message-delivery-goes-through-adapter-test
  (let [runtime (choreo/create (shell/create))
        start (choreo/start-execution!
               runtime :execution/message
               (browser-execution (receive-once)))
        ref (:execution-ref start)
        envelope (machine/message :server :browser :server/message {:answer 42})]
    (is (choreo/active? runtime :execution/message))
    (choreo/deliver-message! runtime ref :physical/message-1 envelope)
    (is (false? (choreo/active? runtime :execution/message)))
    (is (invariant-clean? runtime))))

(deftest stale-participant-message-cannot-resume-replacement-generation-test
  (let [runtime (choreo/create (shell/create))
        first-start (choreo/start-execution!
                     runtime :execution/message-replaced
                     (browser-execution (receive-once)))
        old-ref (:execution-ref first-start)
        _ (choreo/start-execution!
           runtime :execution/message-replaced
           (browser-execution (receive-once))
           {:replace-execution? true})
        new-ref (choreo/execution-ref runtime :execution/message-replaced)
        envelope (machine/message :server :browser :server/message {:answer 42})
        stale (choreo/deliver-message!
               runtime old-ref :physical/stale-message envelope)]
    (is (= :stale-execution-generation
           (get-in stale [:effects 0 1 :reason])))
    (is (choreo/active? runtime :execution/message-replaced))
    (choreo/deliver-message!
     runtime new-ref :physical/current-message envelope)
    (is (false? (choreo/active? runtime :execution/message-replaced)))
    (is (invariant-clean? runtime))))

(deftest external-callback-api-requires-an-adapter-issued-reference-shape-test
  (let [runtime (choreo/create (shell/create))
        envelope (machine/environment-event :browser :browser/continue)]
    (doseq [bad-ref [nil
                     {}
                     {:execution-id :e :generation 1}
                     {:gesso.live.browser.choreo/type choreo/execution-ref-type
                      :execution-id :e
                      :generation 0}]]
      (is (= :invalid-execution-ref
             (:error/kind
              (thrown-data
               #(choreo/deliver-environment! runtime bad-ref envelope))))))))

;; =============================================================================
;; Timer ownership remains adapter + shell
;; =============================================================================

(deftest timer-schedule-and-fire-use-adapter-generation-test
  (let [scheduled (atom nil)
        shell-runtime
        (shell/create
         {:set-timeout!
          (fn [callback delay-ms]
            (reset! scheduled {:callback callback :delay-ms delay-ms})
            :physical/handle)
          :clear-timeout! (fn [_] nil)})
        runtime (choreo/create shell-runtime)
        start (choreo/start-execution!
               runtime :execution/timer
               (browser-execution (await-timeout)))
        ref (:execution-ref start)
        envelope (machine/environment-event :browser :browser/timeout)]
    (choreo/schedule! runtime ref :timer/one 25 envelope)
    (is (= 25 (:delay-ms @scheduled)))
    (is (choreo/active? runtime :execution/timer))
    ((:callback @scheduled))
    (is (false? (choreo/active? runtime :execution/timer)))
    (is (invariant-clean? runtime))))

(deftest timer-cancel-removes-physical-resource-and-late-fire-is-stale-test
  (let [scheduled (atom nil)
        cleared (atom [])
        shell-runtime
        (shell/create
         {:set-timeout!
          (fn [callback _delay-ms]
            (reset! scheduled callback)
            :physical/handle)
          :clear-timeout!
          (fn [handle]
            (swap! cleared conj handle))})
        runtime (choreo/create shell-runtime)
        start (choreo/start-execution!
               runtime :execution/cancel-timer
               (browser-execution (await-timeout)))
        ref (:execution-ref start)
        envelope (machine/environment-event :browser :browser/timeout)]
    (choreo/schedule! runtime ref :timer/one 10 envelope)
    (choreo/cancel-timer! runtime ref :timer/one)
    (is (= [:physical/handle] @cleared))
    (is (choreo/active? runtime :execution/cancel-timer))
    ;; Simulate a browser callback which escaped clearTimeout.
    (@scheduled)
    (is (choreo/active? runtime :execution/cancel-timer))
    (is (invariant-clean? runtime))))

;; =============================================================================
;; Start/retire/query API
;; =============================================================================

(deftest start-plan-is-only-a-convenience-over-portable-machine-start-test
  (let [runtime (choreo/create (shell/create))
        executable-plan (project/project (await-once) :browser)
        result (choreo/start-plan! runtime :execution/plan executable-plan)
        ref (:execution-ref result)]
    (is (choreo/execution-ref? ref))
    (is (machine/execution? (choreo/execution runtime :execution/plan)))
    (is (= #{:execution/plan} (choreo/active-execution-ids runtime)))
    (is (invariant-clean? runtime))))

(deftest start-validation-does-not-create-an-alternate-machine-format-test
  (let [runtime (choreo/create (shell/create))]
    (is (= :invalid-machine-execution
           (:error/kind
            (thrown-data
             #(choreo/start-execution! runtime :execution/bad {:not :machine})))))
    (is (= :unknown-start-options
           (:error/kind
            (thrown-data
             #(choreo/start-execution!
               runtime :execution/bad-options
               (browser-execution (await-once))
               {:metadata {}})))))
    (is (= #{} (choreo/active-execution-ids runtime)))))

(deftest retire-submits-generation-bound-retirement-test
  (let [runtime (choreo/create (shell/create))
        start (choreo/start-execution!
               runtime :execution/retire
               (browser-execution (await-once)))
        ref (:execution-ref start)]
    (is (choreo/active? runtime :execution/retire))
    (choreo/retire! runtime ref :test/retire)
    (is (false? (choreo/active? runtime :execution/retire)))
    (is (invariant-clean? runtime))))

(deftest stale-retire-cannot-retire-new-generation-test
  (let [runtime (choreo/create (shell/create))
        first-ref
        (:execution-ref
         (choreo/start-execution!
          runtime :execution/stale-retire
          (browser-execution (await-once))))
        _ (choreo/start-execution!
           runtime :execution/stale-retire
           (browser-execution (await-once))
           {:replace-execution? true})
        current-ref (choreo/execution-ref runtime :execution/stale-retire)
        stale (choreo/retire! runtime first-ref :stale)]
    (is (= :stale-execution-generation
           (get-in stale [:effects 0 1 :reason])))
    (is (choreo/active? runtime :execution/stale-retire))
    (choreo/retire! runtime current-ref :current)
    (is (false? (choreo/active? runtime :execution/stale-retire)))
    (is (invariant-clean? runtime))))

;; =============================================================================
;; Detach / diagnostics
;; =============================================================================

(deftest detach-does-not-retire-semantic-work-test
  (let [shell-runtime (shell/create)
        runtime (choreo/create shell-runtime)
        _ (choreo/start-execution!
           runtime :execution/live
           (browser-execution (await-once)))]
    (is (choreo/active? runtime :execution/live))
    (is (= :detached (choreo/detach! runtime)))
    (is (choreo/active? runtime :execution/live))
    (is (empty? (shell/handlers shell-runtime)))
    (is (invariant-clean? runtime))))

(deftest detach-removes-only-handler-functions-it-installed-test
  (let [shell-runtime (shell/create)
        runtime (choreo/create shell-runtime)
        replacement (fn [_] :replacement)]
    (shell/register-handler! shell-runtime :machine/local replacement)
    (is (= :detached (choreo/detach! runtime)))
    (is (identical? replacement
                    (:machine/local (shell/handlers shell-runtime))))
    (is (not (contains? (shell/handlers shell-runtime) :machine/send)))
    (is (not (contains? (shell/handlers shell-runtime) :transport/send)))))

(deftest diagnostics-exposes-configuration-not-host-functions-or-private-state-test
  (let [local-handler (fn [_] {:value 1})
        fx-handler (fn [_ value] value)
        send-handler (fn [_] {})
        transport-handler (fn [_] :sent)
        runtime
        (choreo/create
         (shell/create)
         {:local-actions {:browser/work local-handler}
          :fx-handlers {:test/fx fx-handler}
          :send-payload send-handler
          :transport-send transport-handler})
        diagnostics (choreo/diagnostics runtime)]
    (is (= #{:browser/work} (:registered-local-actions diagnostics)))
    (is (= #{:test/fx} (:registered-fx-handlers diagnostics)))
    (is (true? (:send-payload-handler? diagnostics)))
    (is (true? (:transport-handler? diagnostics)))
    (is (= choreo/owned-effect-kinds (:attached-effect-kinds diagnostics)))
    (is (not-any? fn? (tree-seq coll? seq diagnostics)))
    (is (not (contains? diagnostics :executions)))
    (is (not (contains? diagnostics :timers)))
    (is (not (contains? diagnostics :terminal-history)))))

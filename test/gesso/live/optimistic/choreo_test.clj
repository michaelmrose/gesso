(ns gesso.live.optimistic.choreo-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.choreo.project :as project]
   [gesso.choreo.verify :as verify]
   [gesso.live.optimistic.choreo :as optimistic-choreo]
   [gesso.live.optimistic.protocol :as protocol]))

;; -----------------------------------------------------------------------------
;; Helpers
;; -----------------------------------------------------------------------------

(defn- global-state
  [state-id]
  (get-in optimistic-choreo/optimistic-command
          [:states state-id]))

(defn- projected-state
  [plan state-id]
  (project/state plan state-id))

(defn- linear-state-ids
  "Follow a projected path whose states have at most one :next edge until return.

   This is intentionally only used for cleanup paths that are linear by design."
  [plan start]
  (loop [state-id start
         seen #{}
         result []]
    (when (contains? seen state-id)
      (throw
       (ex-info "Unexpected cycle while inspecting projected linear path."
                {:state state-id
                 :path result})))
    (let [state (projected-state plan state-id)
          result' (conj result state-id)]
      (if (= :return (:op state))
        result'
        (recur (:next state)
               (conj seen state-id)
               result')))))

;; -----------------------------------------------------------------------------
;; Semantic identity and compiler products
;; -----------------------------------------------------------------------------

(deftest choreography-identity-test
  (testing "the built-in choreography has one stable semantic identity"
    (is (= :gesso.live.optimistic/command
           optimistic-choreo/protocol-name))
    (is (= #{:browser :server}
           (:roles optimistic-choreo/optimistic-command)))
    (is (= :browser/acquire-target-authority
           (:initial optimistic-choreo/optimistic-command))))

  (testing "wire events come from the optimistic protocol"
    (is (= protocol/command-event
           optimistic-choreo/command-event))
    (is (= protocol/settlement-event
           optimistic-choreo/settlement-event))))

(deftest built-in-choreography-verifies-test
  (testing "namespace loading produces an already-verified compiler value"
    (is (verify/verified?
         optimistic-choreo/verified-optimistic-command)))

  (testing "the built-in graph has no verification errors or warnings"
    (is (= {:valid? true
            :error-count 0
            :warning-count 0
            :reachable-state-count 57
            :unreachable-state-count 0
            :terminal-state-count 7}
           (verify/explain
            optimistic-choreo/verified-optimistic-command)))))

(deftest projected-plans-test
  (testing "both endpoint plans are valid projected plans"
    (is (project/projected-plan?
         optimistic-choreo/browser-plan))
    (is (project/projected-plan?
         optimistic-choreo/server-plan)))

  (testing "plans belong to the expected roles"
    (is (= :browser
           (:role optimistic-choreo/browser-plan)))
    (is (= :server
           (:role optimistic-choreo/server-plan))))

  (testing "projecting the verified choreography reproduces the compiler products"
    (is (= optimistic-choreo/browser-plan
           (project/project
            optimistic-choreo/verified-optimistic-command
            :browser)))
    (is (= optimistic-choreo/server-plan
           (project/project
            optimistic-choreo/verified-optimistic-command
            :server))))

  (testing "a projected plan never contains the global :receive operation"
    (is (not-any?
         #(= :receive (:op %))
         (vals (:states optimistic-choreo/browser-plan))))
    (is (not-any?
         #(= :receive (:op %))
         (vals (:states optimistic-choreo/server-plan))))))

;; -----------------------------------------------------------------------------
;; Resource authority
;; -----------------------------------------------------------------------------

(deftest resource-authority-test
  (testing "the global protocol declares separate linear target and snapshot authority"
    (let [resources (:resources optimistic-choreo/optimistic-command)
          target (get resources optimistic-choreo/target-authority-resource)
          snapshot (get resources optimistic-choreo/snapshot-authority-resource)]
      (is (= :browser (:owner target)))
      (is (= true (:linear? target)))
      (is (= true (:terminal-release? target)))
      (is (= :browser (:owner snapshot)))
      (is (= true (:linear? snapshot)))
      (is (= true (:terminal-release? snapshot)))))

  (testing "only the browser projection carries those local resources"
    (is (= #{optimistic-choreo/target-authority-resource
             optimistic-choreo/snapshot-authority-resource}
           (set (keys (:resources optimistic-choreo/browser-plan)))))
    (is (= {}
           (:resources optimistic-choreo/server-plan)))))

;; -----------------------------------------------------------------------------
;; HTTP command / settlement contract
;; -----------------------------------------------------------------------------

(deftest command-message-contract-test
  (let [send-state (global-state :browser/send-command)
        receive-state (global-state :server/receive-command)]
    (testing "browser sends the protocol command over HTTP"
      (is (= :send (:op send-state)))
      (is (= :browser (:from send-state)))
      (is (= :server (:to send-state)))
      (is (= protocol/command-event (:event send-state)))
      (is (= :http (:via send-state)))
      (is (= protocol/command-required-keys
             (:required send-state)))
      (is (= protocol/command-optional-keys
             (:optional send-state)))
      (is (= protocol/command-correlation-keys
             (:correlation send-state))))

    (testing "server receives and binds that same command"
      (is (= :receive (:op receive-state)))
      (is (= :browser (:from receive-state)))
      (is (= :server (:to receive-state)))
      (is (= protocol/command-event (:event receive-state)))
      (is (= :http (:via receive-state)))
      (is (= :command (:bind receive-state))))))

(deftest settlement-message-contract-test
  (let [send-state (global-state :server/send-settlement)
        receive-state (global-state :browser/receive-settlement)]
    (testing "server sends the semantic settlement over HTTP"
      (is (= :send (:op send-state)))
      (is (= :server (:from send-state)))
      (is (= :browser (:to send-state)))
      (is (= protocol/settlement-event (:event send-state)))
      (is (= :http (:via send-state)))
      (is (= protocol/settlement-required-keys
             (:required send-state)))
      (is (= protocol/settlement-optional-keys
             (:optional send-state)))
      (is (= protocol/settlement-correlation-keys
             (:correlation send-state))))

    (testing "browser receives and binds that same settlement"
      (is (= :receive (:op receive-state)))
      (is (= :server (:from receive-state)))
      (is (= :browser (:to receive-state)))
      (is (= protocol/settlement-event (:event receive-state)))
      (is (= :http (:via receive-state)))
      (is (= :settlement (:bind receive-state))))))

(deftest semantic-settlement-outcomes-test
  (testing "the server accepts exactly the protocol settlement outcomes"
    (is (= (zipmap protocol/settlement-outcomes
                   (repeat :server/send-settlement))
           (:branches
            (global-state :server/validate-outcome)))))

  (testing "the browser returns the four semantic settlement outcomes"
    (is (= {:confirmed :browser/return-confirmed
            :reconciled :browser/return-reconciled
            :rejected :browser/return-rejected
            :failed :browser/return-failed}
           (:branches
            (global-state :browser/settled-outcome))))))

;; -----------------------------------------------------------------------------
;; Browser environment interrupts and continuity
;; -----------------------------------------------------------------------------

(deftest command-interrupt-contract-test
  (let [interrupts (:interrupts
                    (global-state :browser/send-command))]
    (testing "only the three authoritative interruption classes can interrupt the request"
      (is (= {optimistic-choreo/request-failed-event
              :browser/recover-request-failed

              optimistic-choreo/timeout-event
              :browser/recover-timeout

              optimistic-choreo/canonical-superseded-event
              :browser/discard-superseded-snapshot}
             interrupts)))))

(deftest continuity-restoration-is-explicitly-awaited-test
  (doseq [[state-id next-id]
          [[:browser/await-settled-continuity
            :browser/cancel-settled-timeout]

           [:browser/await-request-failed-continuity
            :browser/cancel-request-failed-timeout]

           [:browser/await-timeout-continuity
            :browser/cancel-timeout-after-timeout]]]
    (testing (str state-id " waits for actual post-layout restoration")
      (is (= :await
             (:op (global-state state-id))))
      (is (= {optimistic-choreo/continuity-restored-event
              next-id}
             (:events (global-state state-id)))))))

(deftest browser-plan-retains-continuity-restored-environment-event-test
  (testing "the browser projection exposes the continuity completion event"
    (is (contains?
         (:environment-events optimistic-choreo/browser-plan)
         optimistic-choreo/continuity-restored-event)))

  (testing "the server projection has no browser environment events"
    (is (= #{}
           (:environment-events optimistic-choreo/server-plan)))))

;; -----------------------------------------------------------------------------
;; Canonical-wins safety path
;; -----------------------------------------------------------------------------

(deftest canonical-superseded-path-does-not-restore-stale-continuity-test
  (let [path (linear-state-ids
              optimistic-choreo/browser-plan
              :browser/discard-superseded-snapshot)
        machines (->> path
                      (map #(projected-state
                             optimistic-choreo/browser-plan
                             %))
                      (keep :machine)
                      vec)]
    (testing "canonical-wins cleanup discards snapshot and tears down execution"
      (is (= [:browser/discard-superseded-snapshot
              :browser/release-superseded-snapshot-authority
              :browser/cancel-superseded-timeout
              :browser/clear-superseded-pending
              :browser/release-superseded-target
              :browser/release-superseded-target-authority
              :browser/return-superseded]
             path)))

    (testing "it never restores continuity captured before the optimistic projection"
      (is (not-any?
           #{optimistic-choreo/browser-restore-continuity-machine}
           machines)))

    (testing "it does perform all remaining cleanup FX"
      (is (= [optimistic-choreo/browser-discard-snapshot-machine
              optimistic-choreo/browser-cancel-timeout-machine
              optimistic-choreo/browser-clear-pending-machine
              optimistic-choreo/browser-release-target-machine]
             machines)))))

;; -----------------------------------------------------------------------------
;; FX-machine identities
;; -----------------------------------------------------------------------------

(deftest participant-fx-boundary-test
  (testing "browser projection retains browser FX machines"
    (is (= optimistic-choreo/browser-install-projection-machine
           (:machine
            (projected-state
             optimistic-choreo/browser-plan
             :browser/install-projection))))
    (is (= optimistic-choreo/browser-install-canonical-machine
           (:machine
            (projected-state
             optimistic-choreo/browser-plan
             :browser/install-canonical)))))

  (testing "server projection retains only the server execution machine"
    (let [machines
          (->> (:states optimistic-choreo/server-plan)
               vals
               (keep :machine)
               set)]
      (is (= #{optimistic-choreo/server-execute-machine}
             machines)))))

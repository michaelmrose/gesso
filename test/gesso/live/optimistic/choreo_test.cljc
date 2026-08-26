(ns gesso.live.optimistic.choreo-test
  (:require
   [gesso.choreo.core :as choreo]
   [gesso.choreo.identity :as identity]
   [gesso.choreo.machine :as machine]
   [gesso.choreo.realization :as realization]
   [gesso.live.optimistic.choreo :as optimistic-choreo]
   [gesso.live.optimistic.protocol :as protocol]
   #?(:clj [gesso.choreo.project :as project])
   #?(:clj [gesso.choreo.verify :as verify])
   #?(:clj [clojure.test :refer [deftest is testing]]
      :cljs [cljs.test :refer-macros [deftest is testing]])))

(defn- error-data
  [f]
  (try
    (f)
    nil
    (catch #?(:clj Throwable
              :cljs :default) ex
      (ex-data ex))))

(defn- error-kind
  [f]
  (:error/kind
   (error-data f)))

(def command-id
  (identity/command-id "command-42"))

(def execution-id
  (identity/execution-id "execution-7"))

(def other-execution-id
  (identity/execution-id "execution-8"))

(def basis
  {:tx-id 42
   :system-time "2026-08-25T01:00:00Z"})

(def newer-basis
  {:tx-id 43
   :system-time "2026-08-25T01:00:01Z"})

(def command-options
  {:name :request/claim-optimistic
   :operation :request/claim})

(def supersession-options
  {:name :request/claim-supersession
   :authority :request
   :observation :request/current})

(defn- command-envelope
  ([]
   (command-envelope {}))
  ([overrides]
   (protocol/command
    (merge
     {:command-id command-id
      :execution-id execution-id
      :operation :request/claim
      :arguments {:request-id "request-1"}
      :observed-basis basis
      :scope [:request "request-1"]}
     overrides))))

(defn- provisional-envelope
  ([]
   (provisional-envelope {}))
  ([overrides]
   (protocol/provisional
    (merge
     {:command-id command-id
      :execution-id execution-id
      :observed-basis basis
      :projection {:request/status :claimed
                   :request/claimed-by "helper-1"}
      :scope [:request "request-1"]}
     overrides))))

(defn- authoritative-envelope
  ([]
   (authoritative-envelope {}))
  ([overrides]
   (protocol/authoritative
    (merge
     {:presence :present
      :basis newer-basis
      :projection {:request/status :claimed
                   :request/claimed-by "helper-1"}}
     overrides))))

(defn- settlement-envelope
  ([]
   (settlement-envelope {}))
  ([overrides]
   (protocol/settlement
    (merge
     {:command-id command-id
      :execution-id execution-id
      :resolution :confirmed
      :authoritative (authoritative-envelope)
      :outcome :request/claimed}
     overrides))))

(defn- started-command-realization
  []
  (realization/start
   (optimistic-choreo/command-choreography command-options)
   {:entry-values-by-role
    {:browser
     (optimistic-choreo/command-values
      (command-envelope))}}))

(defn- complete-direct-command
  ([]
   (complete-direct-command
    (settlement-envelope)))
  ([settlement]
   (let [command (command-envelope)
         provisional (provisional-envelope)

         started
         (realization/start
          (optimistic-choreo/command-choreography command-options)
          {:entry-values-by-role
           {:browser
            (optimistic-choreo/command-values command)}})

         after-provisional
         (realization/complete-local
          started
          :browser
          {optimistic-choreo/provisional-value-key
           (optimistic-choreo/provisional-value
            command
            provisional)})

         {after-command-send :realization
          command-message-id :message-id
          command-message :message}
         (realization/complete-send
          after-provisional
          :browser
          (optimistic-choreo/command-values command))

         after-command-delivery
         (realization/deliver-message
          after-command-send
          command-message-id)

         after-authority
         (realization/complete-authoritative
          after-command-delivery
          :authority
          {optimistic-choreo/settlement-value-key
           (optimistic-choreo/settlement-value settlement)})

         {after-settlement-send :realization
          settlement-message-id :message-id
          settlement-message :message}
         (realization/complete-send
          after-authority
          :authority
          (optimistic-choreo/settlement-message-values settlement))

         after-settlement-delivery
         (realization/deliver-message
          after-settlement-send
          settlement-message-id)

         completed
         (realization/complete-local
          after-settlement-delivery
          :browser
          {optimistic-choreo/resolution-value-key
           (optimistic-choreo/settlement-resolution
            provisional
            settlement)})]
     {:command command
      :provisional provisional
      :settlement settlement
      :command-message command-message
      :settlement-message settlement-message
      :completed completed})))

(deftest semantic-vocabulary-is-protocol-v3-and-browser-resource-free
  (is (= protocol/command-event
         optimistic-choreo/command-event))
  (is (= protocol/settlement-event
         optimistic-choreo/settlement-event))
  (is (= #{:command-id :execution-id}
         optimistic-choreo/command-correlation-keys))
  (is (= protocol/settlement-resolutions
         optimistic-choreo/direct-terminal-resolutions))

  (testing "optimistic choreography requires an authoritative basis"
    (is (contains?
         optimistic-choreo/semantic-command-required-keys
         protocol/observed-basis-key)))

  (testing "browser mechanics do not appear in the portable semantic vocabulary"
    (let [portable-vocabulary
          (pr-str
           {:required optimistic-choreo/semantic-command-required-keys
            :optional optimistic-choreo/semantic-command-optional-keys
            :provisional optimistic-choreo/provisional-value-key
            :settlement optimistic-choreo/settlement-value-key
            :resolution optimistic-choreo/resolution-value-key})]
      (doseq [forbidden ["snapshot"
                         "continuity"
                         "timer"
                         "dom"
                         "target-authority"]]
        (is (not (re-find (re-pattern forbidden)
                          portable-vocabulary)))))))

(deftest command-choreography-names-the-real-public-model-operation
  (let [program
        (optimistic-choreo/command-choreography
         command-options)

        authority-state
        (choreo/state
         program
         :gesso.live.optimistic/execute-authoritative)]
    (is (= :authoritative
           (:op authority-state)))
    (is (= :authority
           (:role authority-state)))
    (is (= :request/claim
           (:operation authority-state)))
    (is (= optimistic-choreo/semantic-command-required-keys
           (:requires authority-state)))
    (is (= #{optimistic-choreo/settlement-value-key}
           (:outputs authority-state)))))

(deftest command-entry-knowledge-belongs-only-to-browser
  (is (= {:browser
          optimistic-choreo/semantic-command-required-keys}
         (optimistic-choreo/command-entry-knowledge
          command-options)))

  (let [started (started-command-realization)
        browser (realization/execution started :browser)
        authority (realization/execution started :authority)]
    (is (machine/waiting-local? browser))
    (is (machine/waiting-receive? authority))
    (doseq [key optimistic-choreo/semantic-command-required-keys]
      (is (machine/has-execution-value? browser key))
      (is (false?
           (machine/has-execution-value? authority key))))))

(deftest optimistic-command-requires-observed-authoritative-basis
  (is (= :missing-observed-basis
         (error-kind
          #(optimistic-choreo/command-values
            (protocol/command
             {:command-id command-id
              :execution-id execution-id
              :operation :request/claim
              :arguments {:request-id "request-1"}})))))

  (is (= basis
         (get
          (optimistic-choreo/command-values
           (command-envelope))
          protocol/observed-basis-key))))

(deftest operation-correlation-does-not-become-authorization
  (is (= :request/claim
         (:operation
          (optimistic-choreo/require-operation
           :request/claim
           (command-envelope)))))

  (is (= :operation-mismatch
         (error-kind
          #(optimistic-choreo/require-operation
            :request/unclaim
            (command-envelope)))))

  (testing "operation matching remains portable across keyword/string wire naming"
    (is (= :request/claim
           (:operation
            (optimistic-choreo/require-operation
             :request/claim
             (command-envelope)))))))

(deftest command-and-settlement-message-boundaries-enforce-correlation
  (let [program
        (optimistic-choreo/command-choreography
         command-options)

        command-state
        (choreo/state
         program
         :gesso.live.optimistic/send-command)

        settlement-state
        (choreo/state
         program
         :gesso.live.optimistic/send-settlement)]
    (is (= optimistic-choreo/command-correlation-keys
           (:correlation command-state)))
    (is (= optimistic-choreo/command-correlation-keys
           (:correlation settlement-state)))
    (is (= protocol/command-event
           (:event command-state)))
    (is (= protocol/settlement-event
           (:event settlement-state)))
    (is (= optimistic-choreo/settlement-message-required-keys
           (:required settlement-state)))))

(deftest provisional-and-settlement-values-remain-closed-protocol-values
  (let [command (command-envelope)
        provisional (provisional-envelope)
        settlement (settlement-envelope)]
    (is (= (protocol/provisional
            (dissoc provisional
                    protocol/protocol-version-key
                    protocol/authority-key))
           (optimistic-choreo/provisional-value
            command
            provisional)))

    (is (= (protocol/settlement
            (dissoc settlement
                    protocol/protocol-version-key))
           (optimistic-choreo/settlement-value
            settlement)))

    (is (= :confirmed
           (optimistic-choreo/settlement-resolution
            provisional
            settlement)))

    (is (= :settlement-correlation-mismatch
           (error-kind
            #(optimistic-choreo/settlement-resolution
              provisional
              (settlement-envelope
               {:execution-id other-execution-id})))))))

(deftest independent-role-realization-performs-one-short-direct-command
  (let [{:keys [completed
                command-message
                settlement-message
                settlement]}
        (complete-direct-command)

        browser
        (realization/execution
         completed
         :browser)

        authority
        (realization/execution
         completed
         :authority)]

    (is (realization/completed? completed))
    (is (= [] (realization/messages completed)))
    (is (machine/completed? browser))
    (is (machine/completed? authority))

    (testing "the command is actually emitted by the browser before authority learns it"
      (is (= {:kind :message
              :from :browser
              :to :authority
              :event protocol/command-event
              :payload (optimistic-choreo/command-values
                        (command-envelope))
              :via :http}
             command-message))
      (doseq [key optimistic-choreo/semantic-command-required-keys]
        (is (= #{:communicated}
               (machine/execution-provenance-kinds
                authority
                key)))))

    (testing "the authority result is communicated rather than becoming browser authority"
      (is (= :authority
             (:from settlement-message)))
      (is (= :browser
             (:to settlement-message)))
      (is (= protocol/settlement-event
             (:event settlement-message)))
      (is (= settlement
             (get-in settlement-message
                     [:payload optimistic-choreo/settlement-value-key])))
      (is (= #{:authoritative}
             (machine/execution-provenance-kinds
              authority
              optimistic-choreo/settlement-value-key)))
      (is (= #{:communicated}
             (machine/execution-provenance-kinds
              browser
              optimistic-choreo/settlement-value-key))))

    (testing "generic resolution is terminal and distinct from model outcome"
      (is (= :confirmed
             (machine/execution-value
              browser
              optimistic-choreo/resolution-value-key)))
      (is (= :request/claimed
             (:outcome settlement)))
      (is (= {:outcome :gesso.choreo/complete}
             (machine/result browser)))
      (is (= {:outcome :gesso.choreo/complete}
             (machine/result authority))))

    (is (= [:local
            :send
            :deliver
            :authoritative
            :send
            :deliver
            :local]
           (mapv :kind
                 (realization/history completed))))))

(deftest each-direct-settlement-resolution-has-a-terminal-path
  (doseq [resolution protocol/settlement-resolutions]
    (let [settlement
          (case resolution
            (:confirmed :reconciled :already-incorporated)
            (settlement-envelope
             {:resolution resolution})

            :rejected
            (protocol/settlement
             {:command-id command-id
              :execution-id execution-id
              :resolution :rejected
              :reason :not-allowed})

            :failed
            (protocol/settlement
             {:command-id command-id
              :execution-id execution-id
              :resolution :failed
              :reason :operation-failed}))

          {:keys [completed]}
          (complete-direct-command settlement)

          browser
          (realization/execution
           completed
           :browser)]
      (is (realization/completed? completed))
      (is (= resolution
             (machine/execution-value
              browser
              optimistic-choreo/resolution-value-key))))))

(deftest supersession-recovery-is-a-separate-short-browser-execution
  (let [program
        (optimistic-choreo/supersession-choreography
         supersession-options)]
    (is (= #{:browser}
           (choreo/roles program)))
    (is (= :await
           (:op
            (choreo/state
             program
             :gesso.live.optimistic/await-authoritative-reread))))
    (is (= {:browser
            #{optimistic-choreo/provisional-value-key}}
           (optimistic-choreo/supersession-entry-knowledge
            supersession-options)))
    (is (not
         (contains?
          (choreo/roles program)
          :authority)))))

(deftest supersession-reread-establishes-authoritative-provenance
  (let [provisional
        (provisional-envelope)

        authoritative
        (authoritative-envelope)

        event-data
        (optimistic-choreo/authoritative-reread-data
         authoritative)

        started
        (realization/start
         (optimistic-choreo/supersession-choreography
          supersession-options)
         {:entry-values-by-role
          {:browser
           {optimistic-choreo/provisional-value-key
            provisional}}})

        after-reread
        (realization/environment
         started
         :browser
         optimistic-choreo/default-authoritative-observed-event
         event-data)

        browser-after-reread
        (realization/execution
         after-reread
         :browser)

        completed
        (realization/complete-local
         after-reread
         :browser
         {optimistic-choreo/resolution-value-key
          :superseded})

        browser-completed
        (realization/execution
         completed
         :browser)]

    (is (= authoritative
           (get event-data
                optimistic-choreo/reread-authoritative-key)))
    (is (= newer-basis
           (get event-data
                optimistic-choreo/reread-basis-key)))

    (is (= #{:authoritative}
           (machine/execution-provenance-kinds
            browser-after-reread
            optimistic-choreo/reread-authoritative-key)))
    (is (= #{:authoritative}
           (machine/execution-provenance-kinds
            browser-after-reread
            optimistic-choreo/reread-basis-key)))

    (is (realization/completed? completed))
    (is (= :superseded
           (machine/execution-value
            browser-completed
            optimistic-choreo/resolution-value-key)))
    (is (= [:environment :local]
           (mapv :kind
                 (realization/history completed))))))

(deftest authoritative-reread-data-cannot-invent-or-separate-basis
  (let [authoritative
        (authoritative-envelope)

        event-data
        (optimistic-choreo/authoritative-reread-data
         authoritative)]
    (is (= newer-basis
           (get event-data
                optimistic-choreo/reread-basis-key)))
    (is (= newer-basis
           (get-in event-data
                   [optimistic-choreo/reread-authoritative-key
                    protocol/basis-key]))))

  (is (= :unknown-fields
         (error-kind
          #(optimistic-choreo/authoritative-reread-data
            (assoc
             (authoritative-envelope)
             :claimed-basis
             {:tx-id 999}))))))

(deftest command-and-supersession-options-are-closed
  (is (= :unknown-option
         (error-kind
          #(optimistic-choreo/command-choreography
            (assoc command-options
                   :snapshot-authority
                   :browser)))))
  (is (= :unknown-option
         (error-kind
          #(optimistic-choreo/supersession-choreography
            (assoc supersession-options
                   :continuity-generation
                   4)))))
  (is (= :same-role
         (error-kind
          #(optimistic-choreo/command-choreography
            (assoc command-options
                   :authority-role
                   :browser))))))

#?(:clj
   (deftest verified-artifacts-project-one-independent-plan-per-role
     (let [verified
           (optimistic-choreo/verified-command
            command-options)

           plans
           (optimistic-choreo/command-plans
            command-options)]
       (is (verify/verified? verified))
       (is (= #{:browser :authority}
              (set (keys plans))))
       (doseq [[role plan] plans]
         (is (project/executable-plan? plan))
         (is (= role (:role plan))))

       (testing "projected plans contain no obsolete browser resource vocabulary"
         (let [artifact-text
               (pr-str plans)]
           (doseq [forbidden ["snapshot-authority"
                              "target-authority"
                              "continuity-generation"
                              "browser-restore-continuity"
                              "browser-capture-continuity"]]
             (is (not (re-find (re-pattern forbidden)
                               artifact-text)))))))))

#?(:clj
   (deftest supersession-verifies-and-projects-without-a-suspended-authority-role
     (let [verified
           (optimistic-choreo/verified-supersession
            supersession-options)

           plan
           (optimistic-choreo/supersession-plan
            supersession-options)]
       (is (verify/verified? verified))
       (is (project/executable-plan? plan))
       (is (= :browser (:role plan)))
       (is (= #{:browser}
              (choreo/roles
               (:choreography verified)))))))

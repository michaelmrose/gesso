(ns gesso.model.tx-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [com.biffweb.core :as biff.core]
   [gesso.live.core :as live]
   [gesso.model.command :as command]
   [gesso.model.tx :as tx]))

(def version
  {:revision-key :widget/revision
   :created-at-key :widget/created-at
   :updated-at-key :widget/updated-at})

(def widget-before
  {:xt/id :widget-1
   :widget/name "Before"
   :widget/revision 3
   :widget/created-at :t0
   :widget/updated-at :t1})

(def widget-after
  {:xt/id :widget-1
   :widget/name "After"
   :widget/revision 4
   :widget/created-at :t0
   :widget/updated-at :t2})

(def widget-new
  {:xt/id :widget-2
   :widget/name "New"
   :widget/revision 0
   :widget/created-at :t2
   :widget/updated-at :t2})

(def account-before
  {:xt/id :account-1
   :account/name "Account"
   :widget/revision 8
   :widget/created-at :t0
   :widget/updated-at :t3})

(def account-after
  {:xt/id :account-1
   :account/name "Account changed"
   :widget/revision 9
   :widget/created-at :t0
   :widget/updated-at :t4})

(def widget-create
  (command/create
   :widget
   widget-new
   version))

(def widget-update
  (command/update-command
   :widget
   :rename
   widget-before
   widget-after
   version))

(def account-update
  (command/update-command
   :account
   :rename
   account-before
   account-after
   version))

(def widget-guard
  (command/guard
   :widget
   widget-before
   version))

(def account-guard
  (command/guard
   :account
   account-before
   version))

(def widget-change
  {:topic :widget
   :id :widget-1
   :change/kind :updated})

(def account-change
  {:topic :account
   :id :account-1
   :change/kind :updated})

(defn error-type
  [f]
  (try
    (f)
    nil
    (catch clojure.lang.ExceptionInfo ex
      (:error/type
       (ex-data ex)))))

(deftest assertion-helper-test
  (testing "COUNT(*) uses the corrected HoneySQL aggregate form"
    (is
     (=
      {:assert
       [:= 0
        {:select
         [[[:count '*]]]

         :from
         'widget

         :where
         [:= :widget/name "Widget"]}]}
      (tx/assert-none
       :widget
       [:= :widget/name "Widget"])))

    (is
     (=
      {:assert
       [:= 1
        {:select
         [[[:count '*]]]

         :from
         'widget

         :where
         [:= :widget/name "Widget"]}]}
      (tx/assert-one
       :widget
       [:= :widget/name "Widget"])))

    (is
     (=
      {:assert
       [:>= 1
        {:select
         [[[:count '*]]]

         :from
         'widget

         :where
         [:= :widget/name "Widget"]}]}
      (tx/assert-at-most-one
       :widget
       [:= :widget/name "Widget"]))))

  (testing "document existence helpers add the identity predicate"
    (is
     (=
      (tx/assert-none
       :widget
       [:= :xt/id :widget-2])
      (tx/assert-document-absent
       :widget
       :widget-2)))

    (is
     (=
      (tx/assert-one
       :widget
       [:= :xt/id :widget-1])
      (tx/assert-document-exists
       :widget
       :widget-1))))

  (testing "assert-document-current uses every declared expected-version check"
    (is
     (=
      {:assert
       [:= 1
        {:select
         [[[:count '*]]]

         :from
         'widget

         :where
         [:and
          [:= :xt/id :widget-1]
          [:= :widget/revision 3]
          [:= :widget/updated-at :t1]]}]}
      (tx/assert-document-current
       :widget
       (:model/expected
        widget-update)))))

  (testing "assertion helpers reject invalid generic targets"
    (is
     (=
      ::tx/invalid-entity-type
      (error-type
       #(tx/assert-document-absent
         "widget"
         :widget-1))))

    (is
     (=
      ::tx/invalid-document-id
      (error-type
       #(tx/assert-document-absent
         :widget
         nil))))))

(deftest fragment-test
  (testing "empty fragment is the composition identity"
    (is
     (=
      {:commands []
       :guards []
       :assertions []
       :changes []}
      tx/empty-fragment))

    (is
     (=
      tx/empty-fragment
      (tx/fragment
       {}))))

  (testing "fragment canonicalizes sequential values to vectors"
    (is
     (=
      {:commands
       [widget-update]

       :guards
       [account-guard]

       :assertions
       [(tx/assert-none
         :widget
         [:= :widget/name "duplicate"])]

       :changes
       [widget-change]}
      (tx/fragment
       {:commands
        (list
         widget-update)

        :guards
        (list
         account-guard)

        :assertions
        (list
         (tx/assert-none
          :widget
          [:= :widget/name "duplicate"]))

        :changes
        (list
         widget-change)}))))

  (testing "fragment validates every contained representation"
    (is
     (=
      ::command/invalid-command
      (error-type
       #(tx/fragment
         {:commands
          [{:not
            :a-command}]}))))

    (is
     (=
      ::command/invalid-guard
      (error-type
       #(tx/fragment
         {:guards
          [{:not
            :a-guard}]}))))

    (is
     (=
      ::tx/invalid-assertions
      (error-type
       #(tx/fragment
         {:assertions
          [{:not
            :an-assertion}]}))))

    (is
     (=
      ::tx/invalid-changes
      (error-type
       #(tx/fragment
         {:changes
          [:not-a-map]})))))

  (testing "unknown fragment keys are rejected"
    (is
     (=
      ::tx/unknown-fragment-keys
      (error-type
       #(tx/fragment
         {:commands
          [widget-update]

          :writes
          []})))))

  (testing "fragment collection values must be sequential"
    (is
     (=
      ::tx/invalid-fragment
      (error-type
       #(tx/fragment
         {:commands
          #{widget-update}})))))

  (testing "fragment itself must be a map"
    (is
     (=
      ::tx/invalid-fragment
      (error-type
       #(tx/fragment
         [:commands
          [widget-update]]))))))

(deftest fragment-convenience-test
  (is
   (=
    {:commands
     [widget-update]

     :guards
     []

     :assertions
     []

     :changes
     []}
    (tx/commands-fragment
     widget-update)))

  (is
   (=
    {:commands
     []

     :guards
     [widget-guard]

     :assertions
     []

     :changes
     []}
    (tx/guards-fragment
     widget-guard)))

  (let [assertion
        (tx/assert-document-absent
         :widget
         :widget-9)]
    (is
     (=
      {:commands
       []

       :guards
       []

       :assertions
       [assertion]

       :changes
       []}
      (tx/assertions-fragment
       assertion))))

  (is
   (=
    {:commands
     []

     :guards
     []

     :assertions
     []

     :changes
     [widget-change]}
    (tx/changes-fragment
     widget-change))))

(deftest compose-test
  (let [assertion-a
        (tx/assert-none
         :widget
         [:= :widget/name "a"])

        assertion-b
        (tx/assert-none
         :account
         [:= :account/name "b"])

        result
        (tx/compose
         {:commands
          [widget-update]

          :assertions
          [assertion-a]

          :changes
          [widget-change]}

         {:commands
          [account-update]

          :guards
          [account-guard]

          :assertions
          [assertion-b]

          :changes
          [account-change]})]

    (testing "composition preserves contribution order"
      (is
       (=
        [widget-update
         account-update]
        (:commands
         result)))

      (is
       (=
        [account-guard]
        (:guards
         result)))

      (is
       (=
        [assertion-a
         assertion-b]
        (:assertions
         result)))

      (is
       (=
        [widget-change
         account-change]
        (:changes
         result))))

    (testing "composition intentionally does not prematurely reject duplicate targets"
      (is
       (=
        [widget-update
         widget-update]
        (:commands
         (tx/compose
          {:commands
           [widget-update]}

          {:commands
           [widget-update]})))))))

(deftest target-test
  (is
   (=
    [:widget
     :widget-1]
    (tx/command-target
     widget-update)))

  (is
   (=
    [:widget
     :widget-1]
    (tx/guard-target
     widget-guard))))

(deftest normalize-guards-test
  (testing "identical repeated guards collapse in first-seen order"
    (is
     (=
      [widget-guard
       account-guard]
      (tx/normalize-guards
       [widget-guard
        widget-guard
        account-guard
        widget-guard]))))

  (testing "conflicting snapshots of one dependency are rejected"
    (let [newer-widget
          (assoc
           widget-before
           :widget/revision
           4
           :widget/updated-at
           :t2)

          conflicting
          (command/guard
           :widget
           newer-widget
           version)]

      (is
       (=
        ::tx/conflicting-guards
        (error-type
         #(tx/normalize-guards
           [widget-guard
            conflicting]))))))

  (testing "guards collection must be sequential"
    (is
     (=
      ::tx/invalid-guards
      (error-type
       #(tx/normalize-guards
         #{widget-guard}))))))

(deftest effective-guards-test
  (testing "a guard identical to an update command's expected version is redundant"
    (is
     (=
      [account-guard]
      (tx/effective-guards
       [widget-update]
       [widget-guard
        account-guard]))))

  (testing "a guard for a create target is contradictory"
    (let [guard
          (command/guard
           :widget
           (assoc
            widget-new
            :widget/revision
            2
            :widget/updated-at
            :later)
           version)]
      (is
       (=
        ::tx/guard-conflicts-with-create
        (error-type
         #(tx/effective-guards
           [widget-create]
           [guard]))))))

  (testing "a guard conflicting with an update command's expected version fails"
    (let [newer
          (assoc
           widget-before
           :widget/revision
           4
           :widget/updated-at
           :later)

          conflicting
          (command/guard
           :widget
           newer
           version)]
      (is
       (=
        ::tx/guard-conflicts-with-command
        (error-type
         #(tx/effective-guards
           [widget-update]
           [conflicting]))))))

  (testing "duplicate mutation targets are rejected"
    (is
     (=
      ::tx/duplicate-command-targets
      (error-type
       #(tx/effective-guards
         [widget-update
          widget-update]
         []))))))

(deftest command-translation-test
  (testing "create commands assert absence and put the command document"
    (is
     (=
      (tx/assert-document-absent
       :widget
       :widget-2)
      (tx/command-precondition
       widget-create)))

    (is
     (=
      [:put-docs
       :widget
       widget-new]
      (tx/command->tx-op
       widget-create))))

  (testing "update commands assert their expected version and put the result"
    (is
     (=
      (tx/assert-document-current
       :widget
       (:model/expected
        widget-update))
      (tx/command-precondition
       widget-update)))

    (is
     (=
      [:put-docs
       :widget
       widget-after]
      (tx/command->tx-op
       widget-update))))

  (testing "guards translate through the exact same current-version assertion"
    (is
     (=
      (tx/assert-document-current
       :account
       (:model/expected
        account-guard))
      (tx/guard-assertion
       account-guard)))))

(deftest transaction-ops-order-test
  (let [explicit-a
        (tx/assert-none
         :widget
         [:= :widget/name "reserved"])

        explicit-b
        (tx/assert-at-most-one
         :account
         [:= :account/name "Account"])

        plan
        {:commands
         [widget-create
          account-update]

         :guards
         [widget-guard]

         :assertions
         [explicit-a
          explicit-b]}

        ops
        (tx/transaction-ops
         plan)]

    (testing "all pre-write assertions are emitted before every write"
      (is
       (=
        [explicit-a
         explicit-b
         (tx/guard-assertion
          widget-guard)
         (tx/command-precondition
          widget-create)
         (tx/command-precondition
          account-update)
         (tx/command->tx-op
          widget-create)
         (tx/command->tx-op
          account-update)]
        ops)))

    (testing "the order makes the transaction's pre-write decision evidence inspectable"
      (is
       (every?
        #(contains?
          %
          :assert)
        (take
         5
         ops)))

      (is
       (=
        [:put-docs
         :put-docs]
        (mapv
         first
         (drop
          5
          ops)))))))

(deftest normalize-plan-test
  (testing "publishing plans default to async"
    (is
     (=
      {:commands
       [widget-update]

       :guards
       []

       :assertions
       []

       :changes
       [widget-change]

       :emit
       :async

       :entry
       nil

       :entry-fn
       nil

       :tx-options
       nil}
      (tx/normalize-plan
       {:commands
        [widget-update]

        :changes
        [widget-change]}))))

  (testing "silent plans may omit semantic changes"
    (is
     (=
      false
      (:emit
       (tx/normalize-plan
        {:commands
         [widget-update]

         :emit
         false})))))

  (testing "publishing requires a semantic change"
    (is
     (=
      ::tx/missing-changes
      (error-type
       #(tx/normalize-plan
         {:commands
          [widget-update]})))))

  (testing "a transaction must mutate at least one document"
    (is
     (=
      ::tx/missing-commands
      (error-type
       #(tx/normalize-plan
         {:changes
          [widget-change]})))))

  (testing "duplicate command targets fail at the final plan boundary"
    (is
     (=
      ::tx/duplicate-command-targets
      (error-type
       #(tx/normalize-plan
         {:commands
          [widget-update
           widget-update]

          :changes
          [widget-change]})))))

  (testing "supported emission modes are explicit"
    (doseq [emit
            [:async
             :sync
             false]]
      (is
       (=
        emit
        (:emit
         (tx/normalize-plan
          {:commands
           [widget-update]

           :changes
           (if
            (false?
             emit)
             []
             [widget-change])

           :emit
           emit})))))

    (is
     (=
      ::tx/invalid-emit
      (error-type
       #(tx/normalize-plan
         {:commands
          [widget-update]

          :changes
          [widget-change]

          :emit
          :eventually})))))

  (testing "entry and entry-fn are alternatives"
    (let [entry-fn
          identity]
      (is
       (=
        {:coalesce-key
         [:widget
          :widget-1]}
        (:entry
         (tx/normalize-plan
          {:commands
           [widget-update]

           :changes
           [widget-change]

           :entry
           {:coalesce-key
            [:widget
             :widget-1]}}))))

      (is
       (identical?
        entry-fn
        (:entry-fn
         (tx/normalize-plan
          {:commands
           [widget-update]

           :changes
           [widget-change]

           :entry-fn
           entry-fn}))))

      (is
       (=
        ::tx/ambiguous-entry
        (error-type
         #(tx/normalize-plan
           {:commands
            [widget-update]

            :changes
            [widget-change]

            :entry
            {}

            :entry-fn
            entry-fn}))))))

  (testing "entry, entry-fn, and tx-options are type checked"
    (is
     (=
      ::tx/invalid-entry
      (error-type
       #(tx/normalize-plan
         {:commands
          [widget-update]

          :changes
          [widget-change]

          :entry
          :not-a-map}))))

    (is
     (=
      ::tx/invalid-entry-fn
      (error-type
       #(tx/normalize-plan
         {:commands
          [widget-update]

          :changes
          [widget-change]

          :entry-fn
          :not-callable}))))

    (is
     (=
      ::tx/invalid-tx-options
      (error-type
       #(tx/normalize-plan
         {:commands
          [widget-update]

          :changes
          [widget-change]

          :tx-options
          [:not
           :a-map]})))))

  (testing "unknown plan keys fail rather than being silently ignored"
    (is
     (=
      ::tx/unknown-plan-keys
      (error-type
       #(tx/normalize-plan
         {:commands
          [widget-update]

          :changes
          [widget-change]

          :authorization-versions
          []}))))))

(deftest prepare-test
  (let [validated
        (atom [])

        expected-unformatted
        (tx/transaction-ops
         {:commands
          [widget-update]

          :guards
          []

          :assertions
          []

          :changes
          [widget-change]})]

    (with-redefs
     [biff.core/validate-with-ex
      (fn [documents]
        (swap!
         validated
         conj
         documents)
        documents)]

      (let [prepared
            (tx/prepare
             {}

             {:commands
              [widget-update]

              :changes
              [widget-change]})]

        (testing "Biff 2 validates each put/patch document batch"
          (is
           (=
            [[widget-after]]
            @validated)))

        (testing "HoneySQL assertions are formatted while XTDB vector writes remain unchanged"
          (is
           (=
            (count
             expected-unformatted)
            (count
             (:tx-ops
              prepared))))

          (is
           (vector?
            (first
             (:tx-ops
              prepared))))

          (is
           (string?
            (first
             (first
              (:tx-ops
               prepared)))))

          (is
           (=
            [:put-docs
             :widget
             widget-after]
            (last
             (:tx-ops
              prepared)))))

        (testing "normalized plan is returned alongside prepared operations"
          (is
           (=
            :async
            (get-in
             prepared
             [:plan
              :emit])))

          (is
           (=
            [widget-change]
            (get-in
             prepared
             [:plan
              :changes])))))))

  (testing "prepare no longer requires Biff 1 Malli options in ctx"
    (with-redefs
     [biff.core/validate-with-ex
      identity]

      (is
       (map?
        (tx/prepare
         {}

         {:commands
          [widget-update]

          :changes
          [widget-change]})))))

  (testing "Biff 2 document validation failures propagate"
    (with-redefs
     [biff.core/validate-with-ex
      (fn [_documents]
        (throw
         (ex-info
          "invalid document"
          {:error/type
           ::invalid-document})))]

      (is
       (=
        ::invalid-document
        (error-type
         #(tx/prepare
           {}

           {:commands
            [widget-update]

            :changes
            [widget-change]})))))))

(deftest transact-test
  (let [prepared-plan
        {:commands
         [widget-update]

         :guards
         []

         :assertions
         []

         :changes
         [widget-change]

         :emit
         :sync

         :entry
         nil

         :entry-fn
         identity

         :tx-options
         {:timeout
          123}}

        formatted-ops
        [[:formatted
          :assertion]

         [:formatted
          :write]]

        calls
        (atom [])

        polls
        (atom 0)

        returned-ctx
        {:biff.xtdb/snapshot-token
         "consistent-snapshot"}

        ctx
        {:gesso.live/system
         ::live-system

         :biff.xtdb/poll-now
         #(swap!
           polls
           inc)}]

    (with-redefs
     [tx/prepare
      (fn [actual-ctx actual-plan]
        (swap!
         calls
         conj
         [:prepare
          actual-ctx
          actual-plan])

        {:plan
         prepared-plan

         :tx-ops
         formatted-ops})

      live/transact-and-notify!
      (fn [system actual-ctx options]
        (swap!
         calls
         conj
         [:live
          system
          actual-ctx
          options])

        {:tx
         ::submitted-tx

         :ctx
         returned-ctx})]

      (let [input-plan
            {:input
             :plan}

            result
            (tx/transact!
             ctx
             input-plan)]

        (testing "transact! delegates preparation exactly once"
          (is
           (=
            [:prepare
             ctx
             input-plan]
            (first
             @calls))))

        (testing "Gesso Live receives the formatted transaction plus normalized delivery policy"
          (is
           (=
            [:live
             ::live-system
             ctx
             {:tx-ops
              formatted-ops

              :tx-options
              {:timeout
               123}

              :changes
              [widget-change]

              :emit
              :sync

              :entry
              nil

              :entry-fn
              identity}]
            (second
             @calls))))

        (testing "Live's consistency-aware ctx is preserved"
          (is
           (=
            returned-ctx
            (:ctx
             result)))

          (is
           (=
            ::submitted-tx
            (:tx
             result)))

          (is
           (=
            :committed
            (:commit/status
             result))))

        (testing "the optional Biff listener is nudged after a successful commit"
          (is
           (=
            1
            @polls)))))))

(deftest transact-silent-test
  (let [seen-system
        (atom ::unset)]

    (with-redefs
     [tx/prepare
      (fn [_ctx _plan]
        {:plan
         {:commands
          [widget-update]

          :guards
          []

          :assertions
          []

          :changes
          []

          :emit
          false

          :entry
          nil

          :entry-fn
          nil

          :tx-options
          nil}

         :tx-ops
         [[:formatted
           :write]]})

      live/transact-and-notify!
      (fn [system _ctx _options]
        (reset!
         seen-system
         system)
        {:ctx
         :silent-consistent-ctx})]

      (let [result
            (tx/transact!
             {}
             {:ignored
              :because-prepare-is-stubbed})]

        (testing "silent transactions do not require a Live system"
          (is
           (nil?
            @seen-system)))

        (is
         (=
          :committed
          (:commit/status
           result)))))))

(deftest transact-live-system-test
  (testing "publishing requires a Live system"
    (with-redefs
     [tx/prepare
      (fn [_ctx _plan]
        {:plan
         {:commands
          [widget-update]

          :guards
          []

          :assertions
          []

          :changes
          [widget-change]

          :emit
          :async

          :entry
          nil

          :entry-fn
          nil

          :tx-options
          nil}

         :tx-ops
         []})]

      (is
       (=
        ::tx/missing-live-system
        (error-type
         #(tx/transact!
           {}
           {}))))))

  (testing ":live/system remains an accepted application context key"
    (let [seen
          (atom nil)]

      (with-redefs
       [tx/prepare
        (fn [_ctx _plan]
          {:plan
           {:commands
            [widget-update]

            :guards
            []

            :assertions
            []

            :changes
            [widget-change]

            :emit
            :async

            :entry
            nil

            :entry-fn
            nil

            :tx-options
            nil}

           :tx-ops
           []})

        live/transact-and-notify!
        (fn [system _ctx _options]
          (reset!
           seen
           system)
          {})]

        (tx/transact!
         {:live/system
          ::legacy-live-system}
         {})

        (is
         (=
          ::legacy-live-system
          @seen))))))

(deftest listener-poll-failure-test
  (testing "post-commit listener polling is only a latency optimization"
    (with-redefs
     [tx/prepare
      (fn [_ctx _plan]
        {:plan
         {:commands
          [widget-update]

          :guards
          []

          :assertions
          []

          :changes
          []

          :emit
          false

          :entry
          nil

          :entry-fn
          nil

          :tx-options
          nil}

         :tx-ops
         []})

      live/transact-and-notify!
      (fn [_system _ctx _options]
        {:tx
         ::committed})]

      (is
       (=
        {:tx
         ::committed

         :commit/status
         :committed}
        (tx/transact!
         {:biff.xtdb/poll-now
          #(throw
            (ex-info
             "listener unavailable"
             {}))}
         {}))))))

(deftest module-test
  (testing "the transaction effect is installed exactly as an ordinary Biff FX handler contribution"
    (is
     (=
      {tx/transact-effect
       tx/transact!}
      tx/handlers))

    (is
     (=
      {:biff.fx/handlers
       {tx/transact-effect
        tx/transact!}}
      (tx/module)))))

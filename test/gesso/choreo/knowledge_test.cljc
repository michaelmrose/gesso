(ns gesso.choreo.knowledge-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.choreo.knowledge :as knowledge]))

(defn- error-kind
  [f]
  (try
    (f)
    nil
    (catch #?(:clj Throwable
              :cljs :default) ex
      (:error/kind
       (ex-data ex)))))

(deftest empty-knowledge-belongs-to-one-role
  (let [state
        (knowledge/empty-knowledge
         :browser)]

    (is
     (knowledge/knowledge?
      state))

    (is
     (= :browser
        (knowledge/role
         state)))

    (is
     (= {}
        (knowledge/facts
         state)))

    (is
     (= {}
        (knowledge/values
         state)))

    (is
     (= []
        (knowledge/history
         state)))))

(deftest knowledge-role-must-be-a-keyword
  (is
   (= :invalid-value
      (error-kind
       #(knowledge/empty-knowledge
         "browser")))))

(deftest entry-values-have-input-provenance
  (let [state
        (-> (knowledge/empty-knowledge
             :browser)
            (knowledge/establish-inputs
             {:request-id "request-1"
              :base-revision 41}
             :execution-entry))]

    (is
     (knowledge/known?
      state
      :request-id))

    (is
     (= "request-1"
        (knowledge/value
         state
         :request-id)))

    (is
     (= #{:input}
        (knowledge/provenance-kinds-for
         state
         :request-id)))

    (is
     (= [{:kind :input
          :source :execution-entry}]
        (knowledge/provenance
         state
         :request-id)))))

(deftest authoritative-values-have-authoritative-provenance
  (let [state
        (-> (knowledge/empty-knowledge
             :server)
            (knowledge/establish-authoritative
             {:outcome :confirmed
              :revision 42}
             :request/claim
             {:state :claim}))]

    (is
     (= :confirmed
        (knowledge/value
         state
         :outcome)))

    (is
     (= [{:kind :authoritative
          :operation :request/claim
          :state :claim}]
        (knowledge/provenance
         state
         :outcome)))

    (is
     (= #{:authoritative}
        (knowledge/provenance-kinds-for
         state
         :revision)))))

(deftest asserted-values-remain-distinct-from-authoritative-values
  (let [asserted
        (-> (knowledge/empty-knowledge
             :browser)
            (knowledge/establish-asserted
             {:outcome :confirmed}
             :browser/runtime))

        authoritative
        (-> (knowledge/empty-knowledge
             :server)
            (knowledge/establish-authoritative
             {:outcome :confirmed}
             :request/claim))]

    (is
     (= #{:asserted}
        (knowledge/provenance-kinds-for
         asserted
         :outcome)))

    (is
     (= #{:authoritative}
        (knowledge/provenance-kinds-for
         authoritative
         :outcome)))

    (is
     (not=
      (knowledge/provenance
       asserted
       :outcome)
      (knowledge/provenance
       authoritative
       :outcome)))))

(deftest same-value-may-accumulate-independent-justifications
  (let [state
        (-> (knowledge/empty-knowledge
             :server)
            (knowledge/establish
             :request-id
             "request-1"
             (knowledge/input-provenance
              :route))
            (knowledge/establish
             :request-id
             "request-1"
             (knowledge/asserted-provenance
              :trusted-router)))]

    (is
     (= #{:input :asserted}
        (knowledge/provenance-kinds-for
         state
         :request-id)))

    (is
     (= 2
        (count
         (knowledge/provenance
          state
          :request-id))))

    (is
     (= [:establish :justify]
        (mapv
         :kind
         (knowledge/history
          state))))))

(deftest duplicate-identical-justification-is-idempotent
  (let [provenance
        (knowledge/input-provenance
         :route)

        once
        (knowledge/establish
         (knowledge/empty-knowledge
          :server)
         :request-id
         "request-1"
         provenance)

        twice
        (knowledge/establish
         once
         :request-id
         "request-1"
         provenance)]

    (is
     (= once
        twice))

    (is
     (= 1
        (count
         (knowledge/history
          twice))))

    (is
     (= 1
        (count
         (knowledge/provenance
          twice
          :request-id))))))

(deftest conflicting-value-is-rejected-by-default
  (let [state
        (knowledge/establish
         (knowledge/empty-knowledge
          :server)
         :revision
         41
         (knowledge/input-provenance
          :entry))]

    (is
     (= :knowledge-conflict
        (error-kind
         #(knowledge/establish
           state
           :revision
           42
           (knowledge/authoritative-provenance
            :request/claim)))))))

(deftest generic-replacement-cannot-stand-in-for-authoritative-progression
  (let [before
        (knowledge/establish
         (knowledge/empty-knowledge
          :server)
         :revision
         41
         (knowledge/input-provenance
          :entry))]

    (testing "an authoritative observation may not overwrite a different known value merely because arrival was later"
      (is
       (= :authoritative-progression-required
          (error-kind
           #(knowledge/establish
             before
             :revision
             42
             (knowledge/authoritative-provenance
              :request/claim)
             {:replace? true})))))

    (testing "rejecting the unsupported overwrite leaves the prior knowledge value authoritative-neutral and unchanged"
      (is
       (= 41
          (knowledge/value
           before
           :revision)))

      (is
       (= #{:input}
          (knowledge/provenance-kinds-for
           before
           :revision)))

      (is
       (= 1
          (count
           (knowledge/history
            before)))))))

(deftest communicated-establishment-only-admits-declared-fields
  (let [state
        (knowledge/establish-communicated
         (knowledge/empty-knowledge
          :browser)
         :server
         :request/settled
         #{:outcome :revision}
         {:outcome :confirmed
          :revision 42
          :transport-debug "not semantic"
          :future-field true}
         {:state :receive-settlement
          :via :sse})]

    (is
     (= {:outcome :confirmed
         :revision 42}
        (knowledge/values
         state)))

    (is
     (false?
      (knowledge/known?
       state
       :transport-debug)))

    (is
     (false?
      (knowledge/known?
       state
       :future-field)))

    (is
     (= [{:kind :communicated
          :from :server
          :event :request/settled
          :via :sse
          :state :receive-settlement}]
        (knowledge/provenance
         state
         :outcome)))))

(deftest absent-declared-optional-field-does-not-become-known
  (let [state
        (knowledge/establish-communicated
         (knowledge/empty-knowledge
          :browser)
         :server
         :request/settled
         #{:outcome :revision}
         {:outcome :rejected})]

    (is
     (knowledge/known?
      state
      :outcome))

    (is
     (false?
      (knowledge/known?
       state
       :revision)))))

(deftest communicated-establishment-does-not-revalidate-message-contract
  (let [state
        (knowledge/establish-communicated
         (knowledge/empty-knowledge
          :browser)
         :server
         :request/settled
         #{:outcome :revision}
         {:outcome :confirmed})]

    ;; Message required/optional validation belongs to semantics/project/machine.
    ;; Knowledge only controls which fields may enter role-local knowledge.
    (is
     (= {:outcome :confirmed}
        (knowledge/values
         state)))))

(deftest communicated-establishment-can-explicitly-replace-a-prior-observation
  (let [before
        (knowledge/establish-inputs
         (knowledge/empty-knowledge
          :browser)
         {:revision 41})

        after
        (knowledge/establish-communicated
         before
         :server
         :request/settled
         #{:revision}
         {:revision 42}
         {:replace? true})]

    (is
     (= 42
        (knowledge/value
         after
         :revision)))

    (is
     (= #{:communicated}
        (knowledge/provenance-kinds-for
         after
         :revision)))))

(deftest derived-knowledge-requires-known-dependencies
  (let [state
        (knowledge/establish-inputs
         (knowledge/empty-knowledge
          :browser)
         {:outcome :confirmed})]

    (is
     (= :unknown-dependency
        (error-kind
         #(knowledge/establish-derived
           state
           :display-state
           :confirmed
           :render/display-state
           #{:outcome :revision}))))))

(deftest derived-knowledge-records-rule-and-dependencies
  (let [state
        (-> (knowledge/empty-knowledge
             :browser)
            (knowledge/establish-inputs
             {:outcome :confirmed
              :revision 42})
            (knowledge/establish-derived
             :display-state
             :confirmed
             :render/display-state
             #{:outcome :revision}))]

    (is
     (= :confirmed
        (knowledge/value
         state
         :display-state)))

    (is
     (= [{:kind :derived
          :rule :render/display-state
          :depends-on #{:outcome :revision}}]
        (knowledge/provenance
         state
         :display-state)))))

(deftest derivation-dependency-check-is-role-local
  (let [server
        (knowledge/establish-authoritative
         (knowledge/empty-knowledge
          :server)
         {:outcome :confirmed}
         :request/claim)

        browser
        (knowledge/empty-knowledge
         :browser)]

    (is
     (knowledge/known?
      server
      :outcome))

    (is
     (= :unknown-dependency
        (error-kind
         #(knowledge/establish-derived
           browser
           :display-state
           :confirmed
           :render/display-state
           #{:outcome}))))))

(deftest nil-is-a-valid-known-value
  (let [state
        (knowledge/establish
         (knowledge/empty-knowledge
          :browser)
         :reason
         nil
         (knowledge/input-provenance))]

    (is
     (knowledge/known?
      state
      :reason))

    (is
     (nil?
      (knowledge/value
       state
       :reason)))))

(deftest establish-many-orders-history-deterministically
  (let [state
        (knowledge/establish-many
         (knowledge/empty-knowledge
          :browser)
         {:z 3
          :a 1
          :m 2}
         (knowledge/input-provenance))]

    (is
     (= [:a :m :z]
        (mapv
         :key
         (knowledge/history
          state))))))

(deftest deterministic-history-has-no-wall-clock-fields
  (let [state
        (-> (knowledge/empty-knowledge
             :browser)
            (knowledge/establish-inputs
             {:request-id "request-1"})
            (knowledge/establish-asserted
             {:selected? true}
             :browser/runtime)
            (knowledge/establish-derived
             :view-state
             :selected
             :render/view-state
             #{:selected?}))]

    (doseq [entry
            (knowledge/history state)]

      (is
       (false?
        (contains?
         entry
         :timestamp)))

      (is
       (false?
        (contains?
         entry
         :time)))

      (is
       (false?
        (contains?
         entry
         :instant))))))

(deftest provenance-shapes-are-explicit-and-validated
  (is
   (knowledge/provenance?
    (knowledge/input-provenance)))

  (is
   (knowledge/provenance?
    (knowledge/communicated-provenance
     :server
     :request/settled
     {:via :sse
      :state :receive})))

  (is
   (knowledge/provenance?
    (knowledge/authoritative-provenance
     :request/claim
     {:state :claim})))

  (is
   (knowledge/provenance?
    (knowledge/derived-provenance
     :render/display
     #{:outcome})))

  (is
   (knowledge/provenance?
    (knowledge/asserted-provenance
     :browser/runtime)))

  (is
   (false?
    (knowledge/provenance?
     {:kind :authoritative})))

  (is
   (= :invalid-provenance
      (error-kind
       #(knowledge/establish
         (knowledge/empty-knowledge
          :browser)
         :x
         1
         {:kind :authoritative})))))

(deftest explain-reports-current-role-local-knowledge
  (let [state
        (-> (knowledge/empty-knowledge
             :browser)
            (knowledge/establish-inputs
             {:request-id "request-1"})
            (knowledge/establish-communicated
             :server
             :request/settled
             #{:outcome}
             {:outcome :confirmed}))]

    (is
     (= {:role :browser
         :known-keys #{:request-id :outcome}
         :values {:request-id "request-1"
                  :outcome :confirmed}
         :provenance-kinds-by-key
         {:request-id #{:input}
          :outcome #{:communicated}}
         :history-count 2}
        (knowledge/explain
         state)))))

(deftest unknown-fact-inspection-is-total
  (let [state
        (knowledge/empty-knowledge
         :browser)]

    (is
     (nil?
      (knowledge/fact
       state
       :missing)))

    (is
     (false?
      (knowledge/known?
       state
       :missing)))

    (is
     (nil?
      (knowledge/value
       state
       :missing)))

    (is
     (nil?
      (knowledge/provenance
       state
       :missing)))

    (is
     (= #{}
        (knowledge/provenance-kinds-for
         state
         :missing)))))

;; -----------------------------------------------------------------------------
;; Authoritative observation / reread provenance
;; -----------------------------------------------------------------------------

(deftest authoritative-observation-is-distinct-from-authoritative-operation
  (let [provenance
        (knowledge/authoritative-observation-provenance
         :request/model
         :request/projection
         {:revision 42}
         {:state [:approval :observe]})]

    (is
     (= {:kind :authoritative
         :authority :request/model
         :observation :request/projection
         :basis {:revision 42}
         :state [:approval :observe]}
        provenance))

    (is
     (knowledge/provenance?
      provenance))

    (is
     (not
      (contains?
       provenance
       :operation)))))

(deftest authoritative-observation-requires-an-explicit-authority-observation-and-basis
  (testing "authority and observation identities remain explicit"
    (is
     (= :invalid-value
        (error-kind
         #(knowledge/authoritative-observation-provenance
           "request/model"
           :request/projection
           {:revision 42}))))

    (is
     (= :invalid-value
        (error-kind
         #(knowledge/authoritative-observation-provenance
           :request/model
           "request/projection"
           {:revision 42})))))

  (testing "a missing basis cannot be silently upgraded into authoritative observation"
    (is
     (= :invalid-authoritative-observation
        (error-kind
         #(knowledge/authoritative-observation-provenance
           :request/model
           :request/projection
           nil)))))

  (testing "an arbitrary asserted event is still not authoritative"
    (is
     (= #{:asserted}
        (-> (knowledge/empty-knowledge
             :browser)
            (knowledge/establish-asserted
             {:approved? true}
             :live/invalidation)
            (knowledge/provenance-kinds-for
             :approved?))))))

(deftest authoritative-observation-establishes-authoritative-knowledge
  (let [state
        (knowledge/establish-authoritative-observation
         (knowledge/empty-knowledge
          :browser)
         {:approved? true
          :revision 42}
         :request/model
         :request/projection
         {:revision 42}
         {:state [:approval :observe]})]

    (is
     (= true
        (knowledge/value
         state
         :approved?)))

    (is
     (= 42
        (knowledge/value
         state
         :revision)))

    (is
     (= #{:authoritative}
        (knowledge/provenance-kinds-for
         state
         :approved?)))

    (is
     (= [{:kind :authoritative
          :authority :request/model
          :observation :request/projection
          :basis {:revision 42}
          :state [:approval :observe]}]
        (knowledge/provenance
         state
         :approved?)))))

(deftest same-value-may-gain-authoritative-observation-justification
  (let [before
        (knowledge/establish-inputs
         (knowledge/empty-knowledge
          :browser)
         {:approved? true}
         :initial-render)

        after
        (knowledge/establish-authoritative-observation
         before
         {:approved? true}
         :request/model
         :request/projection
         {:revision 42})]

    (is
     (= true
        (knowledge/value
         after
         :approved?)))

    (is
     (= #{:input :authoritative}
        (knowledge/provenance-kinds-for
         after
         :approved?)))

    (is
     (= 2
        (count
         (knowledge/provenance
          after
          :approved?))))))

(deftest conflicting-authoritative-reread-still-requires-progression
  (let [before
        (knowledge/establish-inputs
         (knowledge/empty-knowledge
          :browser)
         {:revision 41}
         :initial-render)]

    (is
     (= :authoritative-progression-required
        (error-kind
         #(knowledge/establish-authoritative-observation
           before
           {:revision 42}
           :request/model
           :request/projection
           {:revision 42}
           {:replace? true})))))

  (testing "the knowledge layer records the opaque basis but does not invent ordering for it"
    (let [state
          (knowledge/establish-authoritative-observation
           (knowledge/empty-knowledge
            :browser)
           {:revision 42}
           :request/model
           :request/projection
           {:opaque "basis-42"})]
      (is
       (= {:opaque "basis-42"}
          (-> (knowledge/provenance
               state
               :revision)
              first
              :basis))))))

(deftest malformed-authoritative-observation-provenance-is-rejected
  (doseq [malformed
          [{:kind :authoritative
            :authority :request/model
            :observation :request/projection}
           {:kind :authoritative
            :authority :request/model
            :basis {:revision 42}}
           {:kind :authoritative
            :observation :request/projection
            :basis {:revision 42}}
           {:kind :authoritative
            :operation :request/approve
            :authority :request/model
            :observation :request/projection
            :basis {:revision 42}}]]
    (is
     (false?
      (knowledge/provenance?
       malformed)))

    (is
     (= :invalid-provenance
        (error-kind
         #(knowledge/establish
           (knowledge/empty-knowledge
            :browser)
           :approved?
           true
           malformed))))))

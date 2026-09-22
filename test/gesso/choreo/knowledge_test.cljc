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

(defn- attempt
  [f]
  (try
    {:value (f)}
    (catch #?(:clj Throwable
              :cljs :default) ex
      {:error-kind (:error/kind (ex-data ex))
       :error-data (ex-data ex)})))

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
  (let [basis-41 {:revision 41}
        before
        (knowledge/establish-authoritative-observation
         (knowledge/empty-knowledge
          :browser)
         {:revision 41}
         :request/model
         :request/projection
         basis-41)]

    (testing "arrival order and generic replacement are not authoritative progression"
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
      (is
       (= basis-41
          (-> (knowledge/provenance
               before
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

;; -----------------------------------------------------------------------------
;; Authoritative basis progression
;; -----------------------------------------------------------------------------

(defn- basis-progression
  [authority observation from-basis to-basis relation]
  {:kind :authoritative-basis-progression
   :authority authority
   :observation observation
   :from-basis from-basis
   :to-basis to-basis
   :relation relation})

(def basis-41
  {:revision 41
   :tx-id "tx-41"})

(def basis-42
  {:revision 42
   :tx-id "tx-42"})

(def basis-43
  {:revision 43
   :tx-id "tx-43"})

(deftest first-authoritative-observation-may-supersede-non-authoritative-knowledge
  (let [before
        (knowledge/establish-inputs
         (knowledge/empty-knowledge :browser)
         {:approved? false
          :revision 41}
         :initial-render)

        result
        (attempt
         #(knowledge/establish-authoritative-observation
           before
           {:approved? true
            :revision 42}
           :request/model
           :request/projection
           basis-42))

        after
        (:value result)]

    (testing "authority does not need a fictional basis comparison against non-authoritative input"
      (is (nil? (:error-kind result)))
      (when after
        (is (= true
               (knowledge/value after :approved?)))
        (is (= 42
               (knowledge/value after :revision)))
        (is (= #{:authoritative}
               (knowledge/provenance-kinds-for after :approved?)))
        (is (= basis-42
               (-> (knowledge/provenance after :approved?)
                   first
                   :basis)))))))

(deftest distinct-authoritative-bases-require-an-explicit-progression-decision
  (let [before
        (knowledge/establish-authoritative-observation
         (knowledge/empty-knowledge :browser)
         {:approved? false}
         :request/model
         :request/projection
         basis-41)]

    (testing "a later-arriving conflicting observation is not automatically newer"
      (is (= :authoritative-progression-required
             (error-kind
              #(knowledge/establish-authoritative-observation
                before
                {:approved? true}
                :request/model
                :request/projection
                basis-42)))))

    (testing "the rule also applies when the semantic value happens to be unchanged"
      (is (= :authoritative-progression-required
             (error-kind
              #(knowledge/establish-authoritative-observation
                before
                {:approved? false}
                :request/model
                :request/projection
                basis-42)))))))

(deftest advancing-authoritative-basis-can-replace-current-authoritative-knowledge
  (let [before
        (knowledge/establish-authoritative-observation
         (knowledge/empty-knowledge :browser)
         {:approved? false
          :revision 41}
         :request/model
         :request/projection
         basis-41)

        progression
        (basis-progression
         :request/model
         :request/projection
         basis-41
         basis-42
         :advances)

        result
        (attempt
         #(knowledge/establish-authoritative-observation
           before
           {:approved? true
            :revision 42}
           :request/model
           :request/projection
           basis-42
           {:basis-progression progression}))

        after
        (:value result)]

    (is (nil? (:error-kind result)))
    (when after
      (is (= true
             (knowledge/value after :approved?)))
      (is (= 42
             (knowledge/value after :revision)))

      (doseq [key [:approved? :revision]]
        (is (= #{:authoritative}
               (knowledge/provenance-kinds-for after key)))
        (is (= basis-42
               (-> (knowledge/provenance after key)
                   first
                   :basis)))))))

(deftest same-value-advance-moves-the-authoritative-frontier
  (let [at-41
        (knowledge/establish-authoritative-observation
         (knowledge/empty-knowledge :browser)
         {:approved? true}
         :request/model
         :request/projection
         basis-41)

        result-42
        (attempt
         #(knowledge/establish-authoritative-observation
           at-41
           {:approved? true}
           :request/model
           :request/projection
           basis-42
           {:basis-progression
            (basis-progression
             :request/model
             :request/projection
             basis-41
             basis-42
             :advances)}))

        at-42
        (:value result-42)]

    (is (nil? (:error-kind result-42)))

    (when at-42
      (testing "a later conflicting advance must start from the actual current frontier"
        (is (= :authoritative-progression-mismatch
               (error-kind
                #(knowledge/establish-authoritative-observation
                  at-42
                  {:approved? false}
                  :request/model
                  :request/projection
                  basis-43
                  {:basis-progression
                   (basis-progression
                    :request/model
                    :request/projection
                    basis-41
                    basis-43
                    :advances)})))))

      (testing "the same transition succeeds when its witness starts at the advanced frontier"
        (let [result-43
              (attempt
               #(knowledge/establish-authoritative-observation
                 at-42
                 {:approved? false}
                 :request/model
                 :request/projection
                 basis-43
                 {:basis-progression
                  (basis-progression
                   :request/model
                   :request/projection
                   basis-42
                   basis-43
                   :advances)}))

              at-43
              (:value result-43)]
          (is (nil? (:error-kind result-43)))
          (when at-43
            (is (= false
                   (knowledge/value at-43 :approved?)))
            (is (= basis-43
                   (-> (knowledge/provenance at-43 :approved?)
                       first
                       :basis)))))))))

(deftest stale-or-incomparable-authoritative-basis-cannot-replace-current-knowledge
  (let [current
        (knowledge/establish-authoritative-observation
         (knowledge/empty-knowledge :browser)
         {:approved? true}
         :request/model
         :request/projection
         basis-42)]

    (doseq [[incoming relation]
            [[basis-41 :precedes]
             [{:opaque "other-history"} :incomparable]]]
      (is (= :authoritative-basis-not-advancing
             (error-kind
              #(knowledge/establish-authoritative-observation
                current
                {:approved? false}
                :request/model
                :request/projection
                incoming
                {:basis-progression
                 (basis-progression
                  :request/model
                  :request/projection
                  basis-42
                  incoming
                  relation)})))))))

(deftest authoritative-progression-witness-is-closed-and-exactly-bound
  (let [current
        (knowledge/establish-authoritative-observation
         (knowledge/empty-knowledge :browser)
         {:approved? false}
         :request/model
         :request/projection
         basis-41)

        valid
        (basis-progression
         :request/model
         :request/projection
         basis-41
         basis-42
         :advances)]

    (testing "authority, observation, prior basis, and incoming basis are all part of the decision identity"
      (doseq [bad
              [(assoc valid :authority :other/model)
               (assoc valid :observation :other/projection)
               (assoc valid :from-basis {:revision 40})
               (assoc valid :to-basis {:revision 99})]]
        (is (= :authoritative-progression-mismatch
               (error-kind
                #(knowledge/establish-authoritative-observation
                  current
                  {:approved? true}
                  :request/model
                  :request/projection
                  basis-42
                  {:basis-progression bad}))))))

    (testing "malformed or open-ended progression records are rejected rather than partially interpreted"
      (doseq [bad
              [(dissoc valid :relation)
               (assoc valid :relation :later-ish)
               (assoc valid :extra :host-policy)
               {:kind :something-else
                :authority :request/model
                :observation :request/projection
                :from-basis basis-41
                :to-basis basis-42
                :relation :advances}]]
        (is (= :invalid-authoritative-progression
               (error-kind
                #(knowledge/establish-authoritative-observation
                  current
                  {:approved? true}
                  :request/model
                  :request/projection
                  basis-42
                  {:basis-progression bad}))))))))

(deftest progression-decision-cannot-be-reused-across-authoritative-observation-scopes
  (let [current
        (knowledge/establish-authoritative-observation
         (knowledge/empty-knowledge :browser)
         {:approved? false}
         :request/model
         :request/projection
         basis-41)

        request-progression
        (basis-progression
         :request/model
         :request/projection
         basis-41
         basis-42
         :advances)]

    (is (= :authoritative-progression-mismatch
           (error-kind
            #(knowledge/establish-authoritative-observation
              current
              {:approved? true}
              :request/model
              :request/other-projection
              basis-42
              {:basis-progression request-progression}))))))

(deftest equal-basis-cannot-produce-two-different-authoritative-values
  (let [current
        (knowledge/establish-authoritative-observation
         (knowledge/empty-knowledge :browser)
         {:approved? false}
         :request/model
         :request/projection
         basis-42)]

    (is (= :authoritative-basis-conflict
           (error-kind
            #(knowledge/establish-authoritative-observation
              current
              {:approved? true}
              :request/model
              :request/projection
              basis-42))))))


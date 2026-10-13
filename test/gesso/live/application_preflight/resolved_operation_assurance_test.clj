(ns gesso.live.application-preflight.resolved-operation-assurance-test
  "Regression tests for v794's deliberately non-certifying resolved-operation
   indication in source-derived application assurance.

   The public-boundary tests below use the real, freshly checked application
   assembly, operation registry, and physical browser-artifact receipt. Direct
   projection tests use synthetic reports ONLY to test the interpretation of
   incomplete/invalid metadata; those reports cannot establish currentness.

   A Clojure function's metadata is trivially copyable. Neither an exact match
   nor a genuine v792 builder function is proof that the Live :execute! adapter
   obeys the builder's different [ctx input] contract or that any authoritative
   domain rule is correct. All such claims must remain :not-established."
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.live.application-preflight :as application]
   [gesso.live.application-preflight.fixture :as fixture]
   [gesso.live.application-preflight.semantic-audit :as semantic-audit]
   [gesso.model.resolved-operation :as resolved]))

(def ^:private model-premise
  :public-model-operation-contracts-hold)

(def ^:private executor-path
  [:analysis :operation-acquisition-assembly
   :execution-assembly :server :operations])

(def ^:private indication-path
  [:semantic-audit :operation-cards :request/claim
   :resolved-operation-construction])

(def ^:private scope
  resolved/assurance-scope)

(defn- ordinary-executor
  [_]
  {:resolution :rejected :reason :test-only})

(defn- metadata-map
  ([] (metadata-map :request/claim))
  ([operation]
   {:gesso.model/operation-id operation
    :gesso.model/resolution :durable-command-v1
    :gesso.model/assurance-scope scope}))

(defn- executor-with-metadata
  [metadata]
  (with-meta (fn [_] {:resolution :rejected :reason :test-only}) metadata))

(defn- claim-operations
  [executor]
  (assoc (fixture/standard-operations)
         :request/claim
         (fixture/trusted-operation
          :request/claim
          {:published-change-topics #{:request}
           :execute! executor})))

(defn- with-executor
  [executor f]
  (fixture/with-closed-application
    {:operations (claim-operations executor)}
    f))

(defn- indication
  [explanation]
  (get-in explanation indication-path))

(defn- assert-unestablished!
  [result]
  (is (= :untrusted-function-metadata (:evidence-class result)))
  (is (= :metadata-indication-only (:scope result)))
  (is (false? (:construction-authenticated? result)))
  (is (= :not-established (:execution-adapter-correspondence result)))
  (is (= :not-established (:domain-invariants result)))
  (is (= :not-established (:command-replay-guarantee result))))

(defn- assert-model-policy-remains-trusted!
  [explanation]
  (let [audit (:semantic-audit explanation)
        boundary (some #(when (= :request/claim (:operation %)) %)
                       (:opaque-model-boundaries audit))
        model-coverage
        (some #(when (= [:operation :request/claim
                        :model-implementation-conformance]
                       (:subject %)) %)
              (:assurance-coverage audit))]
    (is (= :trusted (:class boundary)))
    (is (= model-premise (:premise boundary)))
    (is (false? (:discharged? boundary)))
    (is (= :trusted (:class model-coverage)))
    (is (= model-premise (:premise model-coverage)))
    (is (= :opaque-authoritative-model-code (:scope model-coverage)))
    (is (false? (get-in audit [:inspection-scope
                               :complete-business-policy-inventory?])))
    (is (= :unknown (get-in audit [:business-rule-catalog :count])))
    (is (not-any? #(contains? #{:proved :certified} (:class %))
                  (:assurance-coverage audit)))))

(defn- synthetic-report
  "Only a projection fixture. Unlike application/explain, this does not check
   application source currentness or artifact correspondence."
  [operation executor]
  {:gesso.live.application-preflight/type
   :gesso.live.application-preflight/report
   :gesso.live.application-preflight/version 3
   :valid? true
   :analysis
   {:operations {operation {:trusted-operation-name 'fixture/operation!}}
    :trusted-assumptions #{model-premise}
    :open-obligations []
    :operation-acquisition-assembly
    {:execution-assembly
     {:server {:operations {operation {:execute! executor}}}}}}})

(defn- direct-indication
  [operation executor]
  (get-in
   (semantic-audit/audit-from-current-report
    (synthetic-report operation executor))
   [:operation-cards operation :resolved-operation-construction]))

(deftest ordinary-registered-executor-does-not-invent-resolution-evidence
  (with-executor
    ordinary-executor
    (fn [{:keys [assembly report]}]
      (let [from-assembly (application/explain assembly)
            from-report (application/explain report)
            result (indication from-assembly)]
        (is (true? (:valid? report)))
        (is (= result (indication from-report)))
        (is (= :no-resolution-construction-metadata (:status result)))
        (is (nil? (:operation-id-consistent? result)))
        (is (= (conj executor-path :request/claim :execute!)
               (:derived-from result)))
        (is (not (contains? result :claimed-checks)))
        (assert-unestablished! result)
        (assert-model-policy-remains-trusted! from-assembly)))))

(deftest genuine-builder-function-is-visible-but-not-a-live-adapter-proof
  (let [invocations (atom [])
        builder (resolved/build-operation
                 {:operation :request/claim
                  :read-record! (fn [& _] (swap! invocations conj :read))
                  :may-disclose? (fn [& _] (swap! invocations conj :disclose))
                  :construct! (fn [& _] (swap! invocations conj :construct))})]
    (is (= (metadata-map) (meta builder)))
    (with-executor
      builder
      (fn [{:keys [assembly report]}]
        (let [explanation (application/explain assembly)
              result (indication explanation)]
          (is (true? (:valid? report)))
          (is (= :consistent-unverified-resolution-metadata
                 (:status result)))
          (is (true? (:operation-id-consistent? result)))
          (is (= (:construction scope) (:claimed-construction result)))
          (is (= (:checked scope) (:claimed-checks result)))
          (is (= (:assumed scope) (:claimed-assumptions result)))
          (is (= (:not-established scope)
                 (:claimed-unestablished result)))
          (is (= result (indication (application/explain report))))
          (assert-unestablished! result)
          (assert-model-policy-remains-trusted! explanation))))
    ;; Application explanation inspects construction metadata; it must never
    ;; call a resolved operation (whose arity does not match Live :execute!).
    (is (empty? @invocations))))

(deftest forged-matching-metadata-has-exactly-the-same-assurance-class
  (let [forged (executor-with-metadata (metadata-map))]
    (with-executor
      forged
      (fn [{:keys [assembly]}]
        (let [explanation (application/explain assembly)
              result (indication explanation)]
          (is (= :consistent-unverified-resolution-metadata
                 (:status result)))
          (is (= (:checked scope) (:claimed-checks result)))
          (is (true? (:operation-id-consistent? result)))
          (assert-unestablished! result)
          (assert-model-policy-remains-trusted! explanation))))))

(deftest mismatched-registered-operation-is-explicitly-inconsistent
  (with-executor
    (executor-with-metadata (metadata-map :request/cancel))
    (fn [{:keys [assembly]}]
      (let [explanation (application/explain assembly)
            result (indication explanation)]
        (is (= :inconsistent-resolution-metadata (:status result)))
        (is (false? (:operation-id-consistent? result)))
        (is (not (contains? result :claimed-checks)))
        (is (not (contains? result :claimed-assumptions)))
        (assert-unestablished! result)
        (assert-model-policy-remains-trusted! explanation)))))

(deftest metadata-is-observed-per-registered-operation-only
  (let [operations (-> (fixture/standard-operations)
                       (assoc :request/cancel
                              (fixture/trusted-operation
                               :request/cancel
                               {:published-change-topics #{}
                                :execute! (executor-with-metadata
                                           (metadata-map :request/cancel))})))]
    (fixture/with-closed-application
      {:operations operations}
      (fn [{:keys [assembly]}]
        (let [explanation (application/explain assembly)
              cards (get-in explanation [:semantic-audit :operation-cards])
              claim (get-in cards [:request/claim
                                   :resolved-operation-construction])
              cancel (get-in cards [:request/cancel
                                    :resolved-operation-construction])]
          (is (= :no-resolution-construction-metadata (:status claim)))
          (is (= :consistent-unverified-resolution-metadata (:status cancel)))
          (is (= [:analysis :operation-acquisition-assembly
                  :execution-assembly :server :operations
                  :request/cancel :execute!]
                 (:derived-from cancel)))
          (assert-unestablished! claim)
          (assert-unestablished! cancel)
          (assert-model-policy-remains-trusted! explanation))))))

(deftest direct-projection-fails-closed-on-missing-or-nonfunction-executors
  (doseq [[description executor expected]
          [[:missing nil :no-inspectable-registered-executor]
           [:nonfunction {:gesso.model/operation-id :request/claim}
            :no-inspectable-registered-executor]
           [:plain-function ordinary-executor
            :no-resolution-construction-metadata]]]
    (testing (name description)
      (let [result (direct-indication :request/claim executor)]
        (is (= expected (:status result)))
        (assert-unestablished! result)
        (is (not (contains? result :claimed-checks)))))))

(deftest direct-projection-rejects-incomplete-or-ill-shaped-metadata
  (let [base (metadata-map)
        cases
        [[:missing-operation (dissoc base :gesso.model/operation-id) false]
         [:missing-kind (dissoc base :gesso.model/resolution) true]
         [:missing-scope (dissoc base :gesso.model/assurance-scope) true]
         [:wrong-operation (assoc base :gesso.model/operation-id
                                  :request/cancel) false]
         [:wrong-kind (assoc base :gesso.model/resolution
                             :durable-command-v2) true]
         [:wrong-construction
          (assoc-in base [:gesso.model/assurance-scope :construction]
                    :unrelated/template) true]
         [:wrong-checked-shape
          (assoc-in base [:gesso.model/assurance-scope :checked]
                    [:not-a-set]) true]
         [:wrong-assumptions-shape
          (assoc-in base [:gesso.model/assurance-scope :assumed]
                    nil) true]
         [:wrong-unestablished-shape
          (assoc-in base [:gesso.model/assurance-scope :not-established]
                    {}) true]
         [:nonmap-scope (assoc base :gesso.model/assurance-scope
                               :not-a-map) true]]]
    (doseq [[description metadata identity-consistent?] cases]
      (testing (name description)
        (let [result (direct-indication
                      :request/claim (executor-with-metadata metadata))]
          (is (= :inconsistent-resolution-metadata (:status result)))
          (is (= identity-consistent? (:operation-id-consistent? result)))
          (assert-unestablished! result)
          (is (not (contains? result :claimed-checks)))
          (is (not (contains? result :claimed-construction))))))))

(deftest unrelated-metadata-cannot-become-resolution-evidence
  (doseq [metadata [{:doc "An ordinary trusted executor"}
                    {:gesso/claimed-certificate :proved
                     :gesso/verified? true}
                    {:gesso.model/resolution-elsewhere :durable-command-v1}]]
    (let [result (direct-indication
                  :request/claim (executor-with-metadata metadata))]
      (is (= :no-resolution-construction-metadata (:status result)))
      (is (nil? (:operation-id-consistent? result)))
      (assert-unestablished! result))))

(deftest consistent-metadata-does-not-trust-claimed-guarantee-content
  (let [claimed (assoc (metadata-map)
                       :gesso.model/assurance-scope
                       {:construction :gesso.model.resolved-operation/operation-v1
                        :checked #{:all-domain-rules-proved}
                        :assumed #{}
                        :not-established #{}})
        result (direct-indication
                :request/claim (executor-with-metadata claimed))]
    ;; This is deliberately *consistent* by the v794 shape predicate.
    ;; The implementation does not authenticate the contents of these sets.
    (is (= :consistent-unverified-resolution-metadata (:status result)))
    (is (= #{:all-domain-rules-proved} (:claimed-checks result)))
    (is (= #{} (:claimed-unestablished result)))
    (assert-unestablished! result)
    (is (= :untrusted-function-metadata (:evidence-class result)))))

(deftest metadata-inspection-is-pure-and-repeatable
  (let [executor (executor-with-metadata
                  (assoc (metadata-map) :doc "Preserved application metadata"))
        supplied (synthetic-report :request/claim executor)
        metadata-before (meta executor)
        first-result (semantic-audit/audit-from-current-report supplied)
        second-result (semantic-audit/audit-from-current-report supplied)]
    (is (= first-result second-result))
    (is (= metadata-before (meta executor)))
    (is (= supplied (synthetic-report :request/claim executor)))
    (is (= :consistent-unverified-resolution-metadata
           (get-in first-result [:operation-cards :request/claim
                                 :resolved-operation-construction :status])))
    (is (= [:analysis :operations :request/claim]
           (get-in first-result [:operation-cards :request/claim
                                 :derived-from])))))

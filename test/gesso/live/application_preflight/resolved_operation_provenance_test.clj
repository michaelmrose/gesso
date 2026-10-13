(ns gesso.live.application-preflight.resolved-operation-provenance-test
  "Permanent regressions for the v798/v799 local Live-adapter construction
   witness as projected by canonical whole-application semantic audit.

   Only the exact executor emitted by build-executor is registered in the
   private runtime construction registry. Copying its metadata or delegating
   to it does not transfer that witness. The witness establishes construction
   provenance only, never authenticated business policy, durable replay,
   transaction guard completeness, or execution correspondence.

   Synthetic reports below test ONLY audit projection. The integration cases
   use a newly checked complete ApplicationAssembly and physical browser
   artifact/receipt from the shared fixture. No test executes the supplied
   authoritative callbacks while obtaining an assurance explanation."
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.live.application-preflight :as application]
   [gesso.live.application-preflight.fixture :as fixture]
   [gesso.live.application-preflight.semantic-audit :as semantic-audit]
   [gesso.live.optimistic.resolved-operation :as adapter]
   [gesso.model.resolved-operation :as model]))

(def ^:private model-premise
  :public-model-operation-contracts-hold)

(def ^:private resolution-key
  :resolved-operation-construction)

(def ^:private executor-root
  [:analysis :operation-acquisition-assembly
   :execution-assembly :server :operations])

(defn- callback-must-not-run
  [calls name]
  (fn [& _]
    (swap! calls conj name)
    (throw (ex-info "Semantic audit must not execute callbacks."
                    {:callback name}))))

(defn- configured-adapter
  "An adapter with compatible v792 metadata. Its callbacks deliberately
   fail if application preflight tries to invoke them."
  [operation calls]
  (let [resolved
        (with-meta
          (callback-must-not-run calls :resolved-operation)
          {:gesso.model/operation-id operation
           :gesso.model/resolution :durable-command-v1
           :gesso.model/assurance-scope model/assurance-scope})]
    (adapter/build-executor
     {:operation operation
      :resolved-operation resolved
      :submission-mode! (callback-must-not-run calls :submission-mode)
      :project! (callback-must-not-run calls :projection)})))

(defn- synthetic-report
  "Projection only: this forged shape supplies no source-currentness proof."
  [operations]
  {:gesso.live.application-preflight/type
   :gesso.live.application-preflight/report
   :gesso.live.application-preflight/version 3
   :valid? true
   :analysis
   {:operations
    (into {}
          (map (fn [operation]
                 [operation {:trusted-operation-name 'fixture/trusted!}]))
          (keys operations))
    :trusted-assumptions #{model-premise}
    :open-obligations []
    :operation-acquisition-assembly
    {:execution-assembly
     {:server {:operations
               (into {}
                     (map (fn [[operation executor]]
                            [operation {:execute! executor}]))
                     operations)}}}}})

(defn- direct-audit
  [operations]
  (semantic-audit/audit-from-current-report
   (synthetic-report operations)))

(defn- card
  [audit operation]
  (get-in audit [:operation-cards operation resolution-key]))

(defn- assert-common-boundary!
  [indication]
  (is (false? (:construction-authenticated? indication)))
  (is (= :not-established (:execution-adapter-correspondence indication)))
  (is (= :not-established (:domain-invariants indication)))
  (is (= :not-established (:command-replay-guarantee indication))))

(defn- assert-not-recognized!
  [indication]
  (is (false? (:adapter-construction-recognized? indication)))
  (is (= :untrusted-function-metadata (:evidence-class indication)))
  (is (= :metadata-indication-only (:scope indication)))
  (is (not (contains? indication :adapter-construction-evidence)))
  (assert-common-boundary! indication))

(defn- assert-recognized!
  [operation indication]
  (is (= :recognized-live-resolved-operation-adapter
         (:status indication)))
  (is (= :private-runtime-construction-registry
         (:evidence-class indication)))
  (is (= :local-adapter-construction-only (:scope indication)))
  (is (= :exact-executor-identity-recognized
         (:adapter-construction-status indication)))
  (is (true? (:operation-id-consistent? indication)))
  (is (true? (:adapter-construction-recognized? indication)))
  (is (= operation (get-in indication
                           [:adapter-construction-evidence :operation])))
  (is (= :private-runtime-construction-registry
         (get-in indication [:adapter-construction-evidence :source])))
  (is (= (:construction adapter/assurance-scope)
         (get-in indication
                 [:adapter-construction-evidence :construction])))
  (is (= adapter/assurance-scope
         (get-in indication
                 [:adapter-construction-evidence :adapter-assurance-scope])))
  (is (= :unverified-function-metadata
         (get-in indication
                 [:adapter-construction-evidence :model-operation-origin])))
  (is (= :not-established
         (get-in indication
                 [:adapter-construction-evidence
                  :application-execution-correspondence])))
  (is (= (:construction model/assurance-scope)
         (:claimed-construction indication)))
  (is (= (:checked model/assurance-scope)
         (:claimed-checks indication)))
  (is (= (:assumed model/assurance-scope)
         (:claimed-assumptions indication)))
  (is (= (:not-established model/assurance-scope)
         (:claimed-unestablished indication)))
  (assert-common-boundary! indication))

(defn- assert-model-conformance-stays-trusted!
  [audit operation]
  (let [boundary
        (some #(when (= operation (:operation %)) %)
              (:opaque-model-boundaries audit))
        coverage
        (some #(when (= [:operation operation
                        :model-implementation-conformance]
                       (:subject %)) %)
              (:assurance-coverage audit))]
    (is (= :trusted (:class boundary)))
    (is (= model-premise (:premise boundary)))
    (is (false? (:discharged? boundary)))
    (is (= :trusted (:class coverage)))
    (is (= model-premise (:premise coverage)))
    (is (= :opaque-authoritative-model-code (:scope coverage)))
    (is (= :unknown (get-in audit [:business-rule-catalog :count])))
    (is (false? (get-in audit
                        [:inspection-scope
                         :complete-business-policy-inventory?])))
    (is (false? (get-in audit
                        [:inspection-scope
                         :zero-undetected-policy-escapes-established?])))
    (is (not-any? #(contains? #{:proved :certified} (:class %))
                  (:assurance-coverage audit)))))

(defn- prepared-operations
  [overrides]
  (reduce-kv
   (fn [operations operation executor]
     (assoc operations operation
            (fixture/trusted-operation
             operation
             {:published-change-topics
              (case operation
                :request/claim #{:request}
                :request/reassign #{:request :request-assignment :audit
                                    :external/audit}
                :request/cancel #{})
              :execute! executor})))
   (fixture/standard-operations)
   overrides))

(defn- with-checked-operations
  [overrides f]
  (fixture/with-closed-application
    {:operations (prepared-operations overrides)}
    f))

(defn- explanation-audit
  [explanation]
  (:semantic-audit explanation))

(deftest actual-adapter-has-exact-local-provenance-only
  (let [calls (atom [])
        executor (configured-adapter :request/claim calls)
        witness (adapter/construction-evidence executor)
        audit (direct-audit {:request/claim executor})
        result (card audit :request/claim)]
    (is (= :constructed-by-live-resolved-operation-adapter
           (:status witness)))
    (is (= :request/claim (:operation witness)))
    (is (= :private-runtime-construction-registry
           (:evidence-source witness)))
    (assert-recognized! :request/claim result)
    (assert-model-conformance-stays-trusted! audit :request/claim)
    (is (empty? @calls))))

(deftest copied-adapter-metadata-is-not-construction-provenance
  (let [calls (atom [])
        real (configured-adapter :request/claim calls)
        forgery (with-meta (fn [_] :forged-executor) (meta real))
        indication (card (direct-audit {:request/claim forgery})
                         :request/claim)]
    (is (not (identical? real forgery)))
    (is (= (meta real) (meta forgery)))
    (is (nil? (adapter/construction-evidence forgery)))
    (is (= :consistent-unverified-resolution-metadata
           (:status indication)))
    (is (true? (:operation-id-consistent? indication)))
    (is (= (:checked model/assurance-scope)
           (:claimed-checks indication)))
    (assert-not-recognized! indication)
    (is (empty? @calls))))

(deftest actual-with-meta-copy-is-not-the-original-executor
  (let [calls (atom [])
        real (configured-adapter :request/claim calls)
        ;; Reapplying identical metadata may return the SAME function; force a
        ;; changed metadata map so we genuinely exercise a different object.
        clone (with-meta real (assoc (meta real) :test/clone true))
        indication (card (direct-audit {:request/claim clone})
                         :request/claim)]
    (is (not (identical? real clone)))
    (is (= (meta real) (dissoc (meta clone) :test/clone)))
    (is (nil? (adapter/construction-evidence clone)))
    (is (= :consistent-unverified-resolution-metadata
           (:status indication)))
    (assert-not-recognized! indication)
    (is (empty? @calls))))

(deftest delegating-wrapper-does-not-transfer-construction-evidence
  (let [calls (atom [])
        real (configured-adapter :request/claim calls)
        wrapper (with-meta (fn [context] (real context)) (meta real))
        result (card (direct-audit {:request/claim wrapper})
                     :request/claim)]
    (is (nil? (adapter/construction-evidence wrapper)))
    (is (= :consistent-unverified-resolution-metadata (:status result)))
    (assert-not-recognized! result)
    ;; No wrapper or underlying executor is ever invoked while explaining.
    (is (empty? @calls))))

(deftest registering-an-adapter-for-the-wrong-operation-is-not-evidence
  (let [calls (atom [])
        cancel (configured-adapter :request/cancel calls)
        report (direct-audit {:request/claim cancel})
        result (card report :request/claim)]
    (is (= :registered-adapter-operation-mismatch (:status result)))
    (is (= :registered-operation-mismatch
           (:adapter-construction-status result)))
    (is (false? (:operation-id-consistent? result)))
    (assert-not-recognized! result)
    (assert-model-conformance-stays-trusted! report :request/claim)
    (is (empty? @calls))))

(deftest a-metadata-only-operation-does-not-inherit-anothers-registry-entry
  (let [calls (atom [])
        claim (configured-adapter :request/claim calls)
        metadata-only (with-meta (fn [_] :unrelated) (meta claim))
        audit (direct-audit {:request/claim claim
                             :request/cancel metadata-only})
        claim-result (card audit :request/claim)
        cancel-result (card audit :request/cancel)]
    (assert-recognized! :request/claim claim-result)
    (is (= :inconsistent-resolution-metadata (:status cancel-result)))
    (is (false? (:operation-id-consistent? cancel-result)))
    (assert-not-recognized! cancel-result)
    (assert-model-conformance-stays-trusted! audit :request/claim)
    (assert-model-conformance-stays-trusted! audit :request/cancel)
    (is (empty? @calls))))

(deftest two-independently-built-adapters-retain-distinct-operation-identities
  (let [calls (atom [])
        claim (configured-adapter :request/claim calls)
        cancel (configured-adapter :request/cancel calls)
        audit (direct-audit {:request/cancel cancel :request/claim claim})]
    (is (not (identical? claim cancel)))
    (assert-recognized! :request/claim (card audit :request/claim))
    (assert-recognized! :request/cancel (card audit :request/cancel))
    (is (= 2 (:operation-count audit)))
    (is (empty? @calls))))

(deftest an-ordinary-executor-remains-unverified
  (let [result (card (direct-audit {:request/claim (fn [_] :plain)})
                     :request/claim)]
    (is (= :no-resolution-construction-metadata (:status result)))
    (is (= :not-established (:adapter-construction-status result)))
    (is (nil? (:operation-id-consistent? result)))
    (assert-not-recognized! result)))

(deftest genuine-model-builder-is-not-a-live-adapter
  (let [calls (atom [])
        builder
        (model/build-operation
         {:operation :request/claim
          :read-record! (callback-must-not-run calls :read)
          :may-disclose? (callback-must-not-run calls :disclose)
          :construct! (callback-must-not-run calls :construct)})
        result (card (direct-audit {:request/claim builder})
                     :request/claim)]
    (is (nil? (adapter/construction-evidence builder)))
    (is (= :consistent-unverified-resolution-metadata (:status result)))
    (assert-not-recognized! result)
    (is (empty? @calls))))

(deftest forged-proof-claims-in-metadata-do-not-become-a-certificate
  (let [calls (atom [])
        real (configured-adapter :request/claim calls)
        forged-scope (assoc model/assurance-scope
                            :checked #{:every-domain-rule-proved}
                            :assumed #{}
                            :not-established #{})
        forged (with-meta (fn [_] :forged)
                 (assoc (meta real)
                        :gesso.model/assurance-scope forged-scope
                        :gesso/claimed-certificate :proved))
        report (direct-audit {:request/claim forged})
        result (card report :request/claim)]
    (is (= :consistent-unverified-resolution-metadata (:status result)))
    (is (= #{:every-domain-rule-proved} (:claimed-checks result)))
    (is (= #{} (:claimed-unestablished result)))
    (is (nil? (adapter/construction-evidence forged)))
    (assert-not-recognized! result)
    (assert-model-conformance-stays-trusted! report :request/claim)
    (is (empty? @calls))))

(deftest malformed-resolved-metadata-cannot-substitute-for-registry-evidence
  (let [calls (atom [])
        real (configured-adapter :request/claim calls)
        cases
        [[:missing-operation (dissoc (meta real)
                                    :gesso.model/operation-id)]
         [:wrong-resolution (assoc (meta real)
                                   :gesso.model/resolution
                                   :imaginary-v99)]
         [:wrong-scope (assoc-in (meta real)
                                 [:gesso.model/assurance-scope :construction]
                                 :imaginary/template)]]]
    (doseq [[description metadata] cases]
      (testing (name description)
        (let [executor (with-meta (fn [_] :forged) metadata)
              result (card (direct-audit {:request/claim executor})
                           :request/claim)]
          (is (= :inconsistent-resolution-metadata (:status result)))
          (is (nil? (adapter/construction-evidence executor)))
          (assert-not-recognized! result)
          (is (not (contains? result :claimed-checks))))))
    (is (empty? @calls))))

(deftest checked-application-explanation-recognizes-only-registered-executor
  (let [calls (atom [])
        executor (configured-adapter :request/claim calls)]
    (with-checked-operations
      {:request/claim executor}
      (fn [{:keys [assembly report]}]
        (let [from-assembly (application/explain assembly)
              from-report (application/explain report)
              audit (explanation-audit from-assembly)
              indication (card audit :request/claim)]
          (is (true? (:valid? report)))
          (is (= (card audit :request/claim)
                 (card (explanation-audit from-report) :request/claim)))
          (is (= indication
                 (card (explanation-audit (application/explain assembly))
                       :request/claim)))
          (is (= (conj executor-root :request/claim :execute!)
                 (:derived-from indication)))
          (assert-recognized! :request/claim indication)
          (assert-model-conformance-stays-trusted! audit :request/claim)
          (is (empty? @calls)))))))

(deftest checked-application-explanation-rejects-a-copied-executor-claim
  (let [calls (atom [])
        real (configured-adapter :request/claim calls)
        imitation (with-meta (fn [_] :pretend) (meta real))]
    (with-checked-operations
      {:request/claim imitation}
      (fn [{:keys [assembly report]}]
        (let [a (application/explain assembly)
              b (application/explain report)
              result (card (explanation-audit a) :request/claim)]
          (is (= result
                 (card (explanation-audit b) :request/claim)))
          (is (= :consistent-unverified-resolution-metadata
                 (:status result)))
          (assert-not-recognized! result)
          (assert-model-conformance-stays-trusted!
           (explanation-audit a) :request/claim)
          (is (empty? @calls)))))))

(deftest checked-application-explanation-rejects-cross-operation-registration
  (let [calls (atom [])
        cancel (configured-adapter :request/cancel calls)]
    (with-checked-operations
      {:request/claim cancel}
      (fn [{:keys [assembly report]}]
        (let [explanation (application/explain assembly)
              result (card (explanation-audit explanation) :request/claim)]
          (is (true? (:valid? report)))
          (is (= :registered-adapter-operation-mismatch (:status result)))
          (is (= :registered-operation-mismatch
                 (:adapter-construction-status result)))
          (assert-not-recognized! result)
          (is (empty? @calls)))))))

(deftest inspecting-a-checked-application-is-not-an-execution-path
  (let [calls (atom [])
        executor (configured-adapter :request/reassign calls)]
    (with-checked-operations
      {:request/reassign executor}
      (fn [{:keys [assembly report]}]
        (let [before (meta executor)
              first-explanation (application/explain assembly)
              second-explanation (application/explain report)
              result (card (explanation-audit first-explanation)
                           :request/reassign)]
          (is (= result
                 (card (explanation-audit second-explanation)
                       :request/reassign)))
          (is (= before (meta executor)))
          (is (= :request/reassign
                 (:operation (adapter/construction-evidence executor))))
          (assert-recognized! :request/reassign result)
          (is (empty? @calls)))))))

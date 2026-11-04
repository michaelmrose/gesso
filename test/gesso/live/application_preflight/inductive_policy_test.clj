(ns gesso.live.application-preflight.inductive-policy-test
  "v1316: permanent adversarial tests for the opt-in v1315 application gate.

   A genuinely closed application, original conditional-induction evidence,
   issued induction-bound executor, and issued Ring handler are supplied by
   the permanent v1295 fixture. No successful proof, plan, or assembly checker
   is mocked on the positive path. Disabled mode is intentionally exercised
   with a substituted checker to establish that no assurance machinery runs.

   Positive strict admission concerns ONE host-supplied source and enumerated
   entry-point snapshot. It is NOT a reusable authority token, a proof of
   production writer coverage or router adoption, or a physical XTDB theorem.
   The host must actually call and honor require-inductive-policy! at startup."
  (:require [clojure.set :as set]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [gesso.assurance.application-induction :as application]
            [gesso.assurance.application-induction-test]
            [gesso.live.application-preflight.inductive-policy :as policy]
            [gesso.model.inductive-plan :as inductive-plan]))

(def ^:private operation :example/claim)
(def ^:private extra-a :example/admin-writer)
(def ^:private extra-b :example/other-writer)
(def ^:private mandatory
  #{:source-bound-conditional-induction
    :exact-issued-handler-and-plan-binding})
(def ^:private optional :no-observed-unclassified-entry-points)
(def ^:private policy-error
  :gesso.live.application-preflight.inductive-policy/invalid-inductive-policy)
(def ^:private application-error
  :gesso.assurance.application-induction/invalid-application-induction)
(def ^:dynamic *source* nil)
(def ^:dynamic *context* nil)

(defn- original-test-var [symbol]
  (or (ns-resolve 'gesso.assurance.application-induction-test symbol)
      (throw (ex-info "Missing permanent v1295 application fixture"
                      {:symbol symbol}))))

(use-fixtures
  :once
  (fn [run]
    ;; Reuse the permanent v1295 source-owning fixture, not the temporary
    ;; v1315 smoke namespace or a fabricated checker result.
    (let [upstream-ns (the-ns 'gesso.assurance.application-induction-test)
          upstream (first (:clojure.test/once-fixtures (meta upstream-ns)))]
      (when-not (fn? upstream)
        (throw (ex-info "Missing v1295 source-owning fixture" {})))
      (upstream (fn []
                  (binding [*source* @(original-test-var '*base*)
                            *context* @(original-test-var '*context*)]
                    (run)))))))

(defn- request
  ([] {:mode :strict :application-source *source* :required mandatory})
  ([source] {:mode :strict :application-source source :required mandatory}))

(defn- inventory-request
  ([] (update (request) :required conj optional))
  ([source] (update (request source) :required conj optional)))

(defn- entry [kind handler] {:kind kind :handler handler})
(defn- alternate-handler [] (fn [_] {:status 200 :body :unregistered}))
(defn- observed-source
  ([other] (observed-source other (into {} (list [operation
                                                 (entry :semantic-route
                                                        (:handler *context*))]))))
  ([other existing]
   (assoc-in *source*
             [:authoritative-coverage-source :enumerate-entry-points!]
             (fn [] (merge existing other)))))

(defn- failure [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn- assert-policy-error! [reason at f]
  (let [data (failure f)]
    (is (map? data) (str "Expected typed rejection " reason))
    (is (= policy-error (:error/type data)) (pr-str data))
    (is (= reason (:reason data)) (pr-str data))
    (is (= at (:at data)) (pr-str data))
    data))

(defn- assert-source-error! [reason f]
  (let [data (failure f)]
    (is (map? data) (str "Expected invalid original source: " reason))
    (is (= application-error (:error/type data)) (pr-str data))
    (is (= reason (:reason data)) (pr-str data))
    data))

(def ^:private forever-open
  #{:independent-induction-checker-soundness
    :host-authoritative-entry-point-inventory-is-complete
    :non-route-xtdb-writers-are-covered
    :deployed-router-and-executors-adopt-checked-snapshot
    :post-check-runtime-bindings-remain-stable
    :physical-prestate-and-initialization-correspondence
    :abstract-to-physical-transition-refinement})

(defn- assert-no-unearned-proof! [report]
  (is (= policy/policy-type (:type report)))
  (is (= policy/policy-version (:version report)))
  (is (= :none (:authority report)))
  (is (= :none (:transaction-authority report)))
  (is (= :not-established (:physical-write-path-completeness report)))
  (is (= :not-established (:authenticated-physical-correspondence report)))
  (is (= :not-established (:universal-xtdb-refinement report)))
  (is (= :not-established (:independent-checker-soundness report)))
  (is (= :not-established (:production-certification report)))
  (is (= :not-established (:deployed-router-adoption report)))
  (is (= :not-established (:post-check-runtime-binding-stability report)))
  (is (set/subset? forever-open (:open-obligations report)))
  (is (not (contains? report :valid?)))
  (is (not (contains? report :proved?)))
  report)

;; Compatibility: the gate is OPT-IN, not a new implicit check on Gesso users.
(deftest nil-policy-preserves-legacy-behavior-without-calling-checker
  (let [calls (atom 0)
        result (with-redefs [application/check-application-induction!
                             (fn [_] (swap! calls inc) (throw (ex-info "called" {})))]
                 (policy/require-inductive-policy! nil))]
    (is (zero? @calls))
    (is (= :disabled (:mode result)))
    (is (= :compatibility (:decision result)))
    (is (= :not-requested-backwards-compatible (:checking result)))
    (is (= :legacy-behavior-unchanged (:scope result)))
    (is (= :not-required (:startup-installation result)))
    (is (= #{} (:required result)))
    (assert-no-unearned-proof! result)))

(deftest explicit-disabled-policy-preserves-legacy-behavior
  (let [result (with-redefs [application/check-application-induction!
                             (fn [_] (throw (ex-info "must not be called" {})))]
                 (policy/check-inductive-policy! {:mode :disabled}))]
    (is (= :compatibility (:decision result)))
    (is (= :disabled (:mode result)))
    (is (nil? (:first-blocker result)))
    (is (= {} (:source-fingerprints result)))
    (is (= #{} (:abstract-operation-ids result)))
    (assert-no-unearned-proof! result)))

(deftest disabled-policy-must-not-accept-hidden-strict-options
  (doseq [r [{:mode :disabled :application-source *source*}
             {:mode :disabled :required mandatory}
             {:mode :disabled :authority :granted}]]
    (assert-policy-error! :wrong-policy-keys []
                          #(policy/require-inductive-policy! r))))

(deftest disabled-policy-with-metadata-is-not-a-trusted-configuration
  (assert-policy-error! :expected-unannotated-plain-map []
                        #(policy/check-inductive-policy!
                          (with-meta {:mode :disabled} {:trusted true}))))

(deftest unsupported-policy-modes-fail-closed
  (doseq [mode [:warn :audit :best-effort nil true false]]
    (assert-policy-error! :unsupported-policy-mode [:mode]
                          #(policy/check-inductive-policy!
                            (assoc (request) :mode mode)))))

(deftest malformed-policy-containers-fail-closed
  (doseq [bad [[] () (sorted-map :mode :strict)
               (with-meta (request) {:approved true})
               [:mode :strict]]]
    (assert-policy-error! :expected-unannotated-plain-map []
                          #(policy/check-inductive-policy! bad))))

(deftest strict-policy-requires-exact-top-level-keys
  (doseq [bad [(dissoc (request) :application-source)
               (dissoc (request) :required)
               (assoc (request) :approved? true)
               (assoc (request) :authority :proof)
               (assoc (request) :prior-report {})]]
    (assert-policy-error! :wrong-policy-keys []
                          #(policy/check-inductive-policy! bad))))

(deftest missing-required-guarantees-cannot-silently-be-inferred
  (doseq [r [#{} #{:source-bound-conditional-induction}
             #{:exact-issued-handler-and-plan-binding}
             #{optional}]]
    (let [data (assert-policy-error!
                :missing-mandatory-guarantees [:required]
                #(policy/check-inductive-policy! (assoc (request) :required r)))]
      (is (= (set/difference mandatory r) (:missing data))))))

(deftest required-guarantees-must-be-an-unannotated-keyword-set
  (doseq [bad [nil [] () {} [:source-bound-conditional-induction]
               #{:source-bound-conditional-induction "invalid"}
               (with-meta mandatory {:certificate true})]]
    (assert-policy-error! :invalid-required-guarantees [:required]
                          #(policy/check-inductive-policy!
                            (assoc (request) :required bad)))))

(deftest unknown-requirements-are-rejected-before-source-checks
  (doseq [unknown [:imaginary-assurance :fake/verified
                   :unlisted-physical-theorem]]
    (let [data (assert-policy-error!
                :unknown-assurance-requirement [:required]
                #(policy/check-inductive-policy!
                  (update (request {:fake-report true})
                          :required conj unknown)))]
      (is (= #{unknown} (:unknown data))))))

(deftest unsupported-physical-and-soundness-guarantees-are-never-granted
  (doseq [unavailable [:authenticated-physical-observation
                       :complete-physical-writer-coverage
                       :universal-xtdb-refinement
                       :independent-induction-checker-soundness
                       :production-certification
                       :deployed-router-adoption
                       :runtime-transaction-authorization]]
    (let [data (assert-policy-error!
                :unavailable-assurance-requirement [:required]
                #(policy/require-inductive-policy!
                  (update (request {:fake-report true})
                          :required conj unavailable)))]
      (is (= #{unavailable} (:unavailable data))))))

(deftest multiple-unsupported-guarantees-are-all-reported
  (let [unavailable #{:universal-xtdb-refinement
                      :production-certification
                      :runtime-transaction-authorization}
        data (assert-policy-error!
              :unavailable-assurance-requirement [:required]
              #(policy/check-inductive-policy!
                (update (request) :required set/union unavailable)))]
    (is (= unavailable (:unavailable data)))
    (is (string? (:message data)))))

(deftest unknown-is-classified-before-unavailable
  (let [data (assert-policy-error!
              :unknown-assurance-requirement [:required]
              #(policy/check-inductive-policy!
                (update (request) :required into
                        [:universal-xtdb-refinement :unrecognized/claim])))]
    (is (= #{:unrecognized/claim} (:unknown data)))))

;; Strict checks must use original issued sources, not a report from elsewhere.
(deftest strict-admission-rechecks-real-application-and-handler
  (let [result (policy/require-inductive-policy! (request))]
    (is (= :strict (:mode result)))
    (is (= :eligible-current-snapshot (:decision result)))
    (is (= :fresh-original-application-induction-rechecked (:checking result)))
    (is (= :one-current-host-supplied-application-and-entry-point-snapshot
           (:scope result)))
    (is (= mandatory (:required result)))
    (is (= #{operation} (:abstract-operation-ids result)))
    (is (nil? (:first-blocker result)))
    (is (= :host-must-invoke-and-honor-gate (:startup-installation result)))
    (is (seq (:original-open-obligation-ids-by-operation result)))
    (is (seq (:open-premises result)))
    (doseq [key [:catalog-fingerprint :induction-fingerprint
                 :certificate-fingerprint :dependency-fingerprint]]
      (is (re-matches #"sha256:[0-9a-f]{64}"
                      (get-in result [:source-fingerprints key]))
          (str "Missing " key)))
    (assert-no-unearned-proof! result)))

(deftest optional-inventory-requirement-is-eligible-for-clean-host-snapshot
  (let [result (policy/require-inductive-policy! (inventory-request))]
    (is (= :eligible-current-snapshot (:decision result)))
    (is (= (conj mandatory optional) (:required result)))
    (is (= #{} (:observed-unclassified-entry-point-ids result)))
    (is (nil? (:first-blocker result)))
    (assert-no-unearned-proof! result)))

(deftest strict-source-relative-result-is-not-a-reusable-admission-token
  (let [report (policy/check-inductive-policy! (request))]
    (is (= :eligible-current-snapshot (:decision report)))
    (is (= :none (:authority report)))
    (is (= :proof (:authority (assoc report :authority :proof))))
    (is (= :not-established (:deployed-router-adoption report)))
    (is (= :eligible-current-snapshot
           (:decision (policy/require-inductive-policy! (request)))))))

(deftest extra-observed-entry-points-are-visible-in-default-strict-mode
  (let [source (observed-source {extra-a (entry :unclassified
                                               (alternate-handler))})
        r (policy/check-inductive-policy! (request source))]
    (is (= :eligible-current-snapshot (:decision r)))
    (is (= #{extra-a} (:observed-unclassified-entry-point-ids r)))
    (is (nil? (:first-blocker r)))
    (is (contains? (:open-obligations r)
                   :host-authoritative-entry-point-inventory-is-complete))
    (assert-no-unearned-proof! r)))

(deftest optional-requirement-blocks-on-one-unclassified-entry-point
  (let [source (observed-source {extra-a (entry :unclassified
                                               (alternate-handler))})
        request (inventory-request source)
        result (policy/check-inductive-policy! request)]
    (is (= :blocked (:decision result)))
    (is (= :entry-point-coverage (get-in result [:first-blocker :class])))
    (is (= :observed-unclassified-entry-point
           (get-in result [:first-blocker :code])))
    (is (= extra-a (get-in result [:first-blocker :entry-point-id])))
    (is (= [:application-source :authoritative-coverage-source
            :enumerate-entry-points!]
           (get-in result [:first-blocker :at])))
    (let [data (assert-policy-error!
                :strict-assurance-requirement-unsatisfied
                [:application-source :authoritative-coverage-source
                 :enumerate-entry-points!]
                #(policy/require-inductive-policy! request))]
      (is (= (:first-blocker result) (:first-blocker data)))
      (is (= (conj mandatory optional) (:required data))))
    (assert-no-unearned-proof! result)))

(deftest multiple-unclassified-entry-points-have-a-deterministic-first-blocker
  (let [entries {extra-b (entry :unclassified (alternate-handler))
                 extra-a (entry :unclassified (alternate-handler))}
        r1 (policy/check-inductive-policy!
            (inventory-request (observed-source entries)))
        r2 (policy/check-inductive-policy!
            (inventory-request
             (observed-source (into (array-map) (reverse (seq entries))))))]
    (is (= :blocked (:decision r1) (:decision r2)))
    (is (= #{extra-a extra-b} (:observed-unclassified-entry-point-ids r1)))
    (is (= extra-a (get-in r1 [:first-blocker :entry-point-id])))
    (is (= (:first-blocker r1) (:first-blocker r2)))))

(deftest fresh-inventory-snapshot-invalidates-a-previous-eligible-report
  (let [snapshot (atom {operation (entry :semantic-route (:handler *context*))})
        source (assoc-in *source*
                         [:authoritative-coverage-source :enumerate-entry-points!]
                         #(deref snapshot))
        request (inventory-request source)
        first-result (policy/check-inductive-policy! request)]
    (is (= :eligible-current-snapshot (:decision first-result)))
    (swap! snapshot assoc extra-a (entry :unclassified (alternate-handler)))
    (let [data (assert-policy-error!
                :strict-assurance-requirement-unsatisfied
                [:application-source :authoritative-coverage-source
                 :enumerate-entry-points!]
                #(policy/require-inductive-policy! request))]
      (is (= extra-a (get-in data [:first-blocker :entry-point-id]))))
    (is (= :none (:authority first-result)))
    (is (= :not-established (:post-check-runtime-binding-stability
                             first-result)))))

(deftest host-inventory-function-runs-again-on-each-strict-check
  (let [calls (atom 0)
        source (assoc-in *source*
                         [:authoritative-coverage-source :enumerate-entry-points!]
                         (fn []
                           (swap! calls inc)
                           {operation (entry :semantic-route
                                             (:handler *context*))}))]
    (is (= :eligible-current-snapshot
           (:decision (policy/check-inductive-policy! (request source)))))
    (is (= 1 @calls))
    (is (= :eligible-current-snapshot
           (:decision (policy/require-inductive-policy! (request source)))))
    (is (= 2 @calls))))

(deftest missing-declared-route-fails-before-inventory-option-evaluation
  (let [source (observed-source {} {})
        data (failure #(policy/require-inductive-policy!
                        (inventory-request source)))]
    (is (= :gesso.live.application-preflight.authoritative-coverage/invalid-authoritative-coverage
           (:error/type data)) (pr-str data))
    (is (= :missing-observed-semantic-routes (:reason data)) (pr-str data))))

(deftest swapped-observed-route-handler-is-not-a-policy-snapshot
  (let [source (observed-source {}
                                {operation (entry :semantic-route (alternate-handler))})
        data (failure #(policy/require-inductive-policy! (request source)))]
    (is (= :gesso.live.application-preflight.authoritative-coverage/invalid-authoritative-coverage
           (:error/type data)) (pr-str data))
    (is (= :substituted-semantic-route-handler (:reason data)) (pr-str data))))

(deftest correct-operation-id-does-not-authorize-a-second-issued-executor
  (let [plan-source (get-in *source* [:inductive-plans operation :current-source])
        another (inductive-plan/build-inductive-plan-operation! plan-source)
        source (assoc-in *source* [:inductive-plans operation :executor] another)]
    (is (not (identical? another (:executor *context*))))
    (assert-source-error! :issued-handler-plan-identity-mismatch
                          #(policy/require-inductive-policy! (request source)))))

(deftest missing-inductive-plan-is-rejected-as-original-source-error
  (assert-source-error!
   :incomplete-or-extra-inductive-plans
   #(policy/require-inductive-policy!
     (request (assoc *source* :inductive-plans {})))))

(deftest unissued-executor-cannot-borrow-operation-identity
  ;; The application identity boundary rejects the substitution even before
  ;; the lower-level issuance predicate has an opportunity to reject it.
  (assert-source-error!
   :issued-handler-plan-identity-mismatch
   #(policy/require-inductive-policy!
     (request (assoc-in *source* [:inductive-plans operation :executor]
                        (fn [_] {:operation operation :authority :proof}))))))

(deftest wrong-original-induction-source-is-rejected
  (let [source (assoc-in *source*
                         [:inductive-plans operation :current-source
                          :inductive-source :current-graph]
                         {:malformed true})]
    (assert-source-error!
     :mismatched-induction-source
     #(policy/require-inductive-policy! (request source)))))

(deftest forged-application-report-cannot-substitute-for-original-source
  (doseq [fake [{:valid? true :authority :none}
                {:type application/report-type
                 :version application/report-version
                 :authority :none
                 :abstract-invariant :source-bound-conditionally-inductive}
                {:decision :eligible-current-snapshot :type policy/policy-type}]]
    (assert-source-error!
     :wrong-source-keys
     #(policy/require-inductive-policy! (request fake)))))

(deftest malformed-original-application-source-is-rejected
  (doseq [bad [nil [] (sorted-map)
               (with-meta *source* {:admitted true})]]
    (assert-source-error!
     :expected-unannotated-plain-map
     #(policy/require-inductive-policy! (request bad)))))

(deftest copied-application-snapshot-is-not-a-source-freshness-bypass
  (let [copy (into {} *source*)
        fake (assoc copy :authority :granted)]
    (assert-source-error!
     :wrong-source-keys
     #(policy/require-inductive-policy! (request fake)))))

(deftest checker-redefinition-cannot-inject-preapproved-application-report
  (assert-policy-error!
   :substituted-upstream-checker [:application-source]
   #(with-redefs [application/check-application-induction!
                   (fn [_] {:valid? true :authority :granted})]
      (policy/require-inductive-policy! (request)))))

(deftest checker-redefinition-is-detected-even-if-report-looks-legitimate
  (let [forged {:type application/report-type
                :version application/report-version
                :checking :fresh-induction-route-coverage-and-issued-plans-rechecked
                :abstract-invariant :source-bound-conditionally-inductive
                :plan-correspondence :exact-issued-executor-and-abstract-construction
                :authority :none
                :transaction-authority :none}]
    (assert-policy-error!
     :substituted-upstream-checker [:application-source]
     #(with-redefs [application/check-application-induction! (fn [_] forged)]
        (policy/check-inductive-policy! (request))))))

(deftest malformed-required-guarantees-are-rejected-before-original-source
  (let [r (-> (request {:valid? true})
              (assoc :required #{}))]
    (assert-policy-error! :missing-mandatory-guarantees [:required]
                          #(policy/require-inductive-policy! r))))

(deftest copied-and-modified-policy-report-has-no-admission-power
  (let [r (policy/check-inductive-policy! (request))
        forged (-> r
                   (assoc :authority :granted
                          :transaction-authority :granted
                          :production-certification :established))]
    (is (= :granted (:authority forged)))
    (is (= :none (:authority r)))
    ;; The host can only enforce a new ORIGINAL-source request, not a report.
    (assert-policy-error! :wrong-policy-keys []
                          #(policy/require-inductive-policy! forged))))

(deftest strict-policy-does-not-infer-authorization-from-clean-observation
  (let [r (policy/check-inductive-policy! (inventory-request))]
    (is (= :eligible-current-snapshot (:decision r)))
    (is (contains? (:open-obligations r)
                   :non-route-xtdb-writers-are-covered))
    (is (contains? (:open-obligations r)
                   :physical-prestate-and-initialization-correspondence))
    (is (= :host-must-invoke-and-honor-gate (:startup-installation r)))
    (assert-no-unearned-proof! r)))

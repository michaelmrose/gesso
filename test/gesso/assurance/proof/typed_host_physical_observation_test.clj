(ns gesso.assurance.proof.typed-host-physical-observation-test
  "v1762: Reuse the existing v1480 local Biff/adapter capture, including a
   controlled positive adapter seam and adversarial absent-observation cases.
   The synthetic XTDB API in the positive case is NOT authenticated." 
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [gesso.assurance.proof.typed-host-physical-observation :as seam]
            [gesso.assurance.proof.typed-certified-transition-alignment :as aligned]
            [gesso.assurance.proof.installed-route-physical-correlation-test]
            [gesso.assurance.proof.checked-biff-host-adoption :as host]
            [gesso.assurance.proof.xtdb-gateway-typed-transition-seam-test]
            [gesso.live.application-preflight :as app]
            [gesso.live.application-preflight.certified-contract :as certified]
            [gesso.live.application-preflight.fixture :as fixture]
            [gesso.model.certified-plan :as plan]))

(def ^:private fixture-ns
  'gesso.assurance.proof.installed-route-physical-correlation-test)
(def ^:private plan-test 'gesso.model.certified-plan-test)
(def ^:private typed-test
  'gesso.assurance.proof.xtdb-gateway-typed-transition-seam-test)
(defn- helper [ns-name name]
  (var-get (or (ns-resolve ns-name name)
               (throw (ex-info "Missing test helper"
                               {:namespace ns-name :name name})))))
(defn- catalog [] (helper typed-test 'declared-catalog))
(def ^:dynamic *modules* [])
(def ^:dynamic *case* nil)

(defn- with-case [run]
  (let [plan-source (:source ((helper plan-test 'environment)))
        executor (plan/build-certified-plan-operation! plan-source)
        mode (atom :submit)
        calls (atom [])
        handler (certified/issue-certified-handler!
                 {:executor executor
                  :current-source plan-source
                  :request->input! (fn [_]
                                     (swap! calls conj :input)
                                     ((helper plan-test 'invocation) @mode))
                  :result->response! (fn [_ result]
                                       (swap! calls conj :response)
                                       {:status 200 :body result})})]
    (fixture/with-closed-application
     {:operations {:example/claim
                   (fixture/trusted-operation
                    :example/claim {:published-change-topics #{}})}}
     (fn [{:keys [assembly]}]
       (let [contract {:application-assembly assembly
                       :semantic-route-handlers {:example/claim handler}
                       :certified-operations
                       {:example/claim {:executor executor
                                        :current-source plan-source}}}
             issued (host/issue-checked-biff-host-module! contract)
             modules [app/application-handler-module (:module issued)]
             system (binding [*modules* modules]
                      (app/start-biff-application!
                       assembly {} #'*modules*
                       [app/application-handler-module-id host/module-id]))]
         (binding [*case* {:contract contract
                          :system system :host (:handle issued)
                          :mode mode :calls calls}]
           (run)))))))
(use-fixtures :once with-case)
(use-fixtures :each (fn [run]
                      (reset! (:mode *case*) :submit)
                      (reset! (:calls *case*) [])
                      (run)))

(defn- issued []
  (seam/issue-typed-host-physical-observer!
   (:host *case*) (:contract *case*) (:system *case*)
   (catalog) :example/claim))
(defn- request []
  {:request-method :post :uri "/operations/example/claim"
   :request-id (random-uuid)})
(defn- reason [thunk]
  (try (thunk) :no-error
       (catch clojure.lang.ExceptionInfo e
         (or (:reason (ex-data e))
             (throw e)))))

(deftest bounded-same-handler-typed-route-and-adapter-agree
  ;; Controlled test TX adapter is intentionally not the real XTDB process.
  ((helper fixture-ns 'artificial-execution) {}
   (fn [{:keys [transactions api-calls]}]
     (let [binder (issued)
           before ((:summary! binder))
           observed ((:observe-once! binder) (request))
           after ((:summary! binder))]
       (is (= :not-attempted (:observation-status before)))
       (is (= :bounded-same-invocation-adapter-correlation (:status observed)))
       (is (= :bounded-installed-route-to-adapter-agreement
              (:physical-observation-status observed)))
       (is (true? (:resolved-executor-attempt-observed? observed)))
       (is (true? (:one-adapter-call-observed? observed)))
       (is (= :bounded-same-invocation-adapter-correlation
              (:observation-status after)))
       (is (= 1 @transactions))
       (is (= 1 (count @api-calls)))
       (is (= :not-established (:typed-gateway-used-by-handler observed)))
       (is (= :not-established (:typed-to-certified-executor-transition-equivalence
                               observed)))
       (is (= :not-established (:original-business-policy-authenticity observed)))
       (is (= :not-established (:xtdb-api-and-receipt-authenticity observed)))
       (is (= :not-issued (:certificate observed)))
       (is (= :none (:authority observed)))
       (is (not-any? #(contains? observed %)
                     [:handle :execution :report :request :response :plan :tx-ops
                      :source :original-catalog :node :witness :result]))
       (is (= :one-shot-observation-already-attempted
              (reason #((:observe-once! binder) (request)))))
       (is (= 1 @transactions) "No second physical call on witness recheck")))))

(deftest no-physical-attempt-never-proves-correspondence
  (reset! (:mode *case*) :reconcile)
  (let [binder (issued)
        report ((:observe-once! binder) (request))]
    (is (= :no-qualified-same-invocation-adapter-correlation
           (:status report)))
    (is (= :missing-or-invalid-executor-attempt
           (:physical-observation-status report)))
    (is (false? (:one-adapter-call-observed? report)))
    (is (= :not-established (:typed-gateway-used-by-handler report)))
    (is (= :not-issued (:certificate report)))
    (is (= [:input :response] @(:calls *case*)))
    (is (= :one-shot-observation-already-attempted
           (reason #((:observe-once! binder) (request)))))))

(deftest wrong-routes-and-preinstalled-observers-reject-before-dispatch
  (doseq [bad [(assoc (request) :request-method :get)
               (assoc (request) :uri "/other")
               (assoc (request) :uri "/operations/example/claim/other")
               (assoc (request) :gesso.model.resolved-operation/observe-transaction!
                      (fn [_] nil))
               (with-meta (request) {:trusted true})]]
    (let [binder (issued)]
      (is (= :request-does-not-select-bound-typed-operation
             (reason #((:observe-once! binder) bad))))
      (is (= :one-shot-observation-already-attempted
             (reason #((:observe-once! binder) (request)))))))
  (is (empty? @(:calls *case*))))

(deftest unissued-host-or-replaced-observer-cannot-claim-physical-result
  (is (= :unissued-host-adoption
         (reason #(seam/issue-typed-host-physical-observer!
                   (host/->HostAdoptionHandle) (:contract *case*)
                   (:system *case*) (catalog) :example/claim))))
  (let [binder (issued)]
    (with-redefs [gesso.assurance.proof.installed-route-physical-correlation/observe-installed-route-physical!
                  (fn [_] {:report {:status :bounded-installed-route-to-adapter-agreement}})]
      (is (= :substituted-host-or-correlation-boundary
             (reason #((:observe-once! binder) (request))))))
    (is (= :one-shot-observation-already-attempted
           (reason #((:observe-once! binder) (request)))))
    (is (empty? @(:calls *case*)))))

;; v1763: The positive catalog is derived from the SAME original construction
;; as the issued v1251 executor. These are still application-supplied rules:
;; matching them never proves that the business policy was complete/correct.
(defn- aligned-catalog []
  {:id :example/certified-transition-catalog
   :declared-operation-ids #{:example/claim}
   :initial-condition
   {:readers #{:request/status}
    :expression [:= [:read :request/status] [:literal :open]]}
   :invariants
   {:request/version-nonnegative
    {:readers #{:request/version}
     :expression [:>= [:read :request/version] [:literal 0]]}}
   :operations [{:id :example/claim
                 :construction
                 (get-in *case* [:contract :certified-operations :example/claim
                                 :current-source :construction])}]})

(defn- aligned-issuer
  ([] (aligned-issuer (aligned-catalog)))
  ([source]
   (aligned/issue-source-aligned-host-physical-observer!
    (:host *case*) (:contract *case*) (:system *case*)
    source :example/claim)))

(deftest exact-original-catalog-source-matches-issued-plan
  ((helper fixture-ns 'artificial-execution) {}
   (fn [{:keys [transactions api-calls]}]
     (let [bound (aligned-issuer)
           before ((:summary! bound))
           observed ((:observe-once! bound) (request))
           after ((:summary! bound))]
       (is (= aligned/report-type (:type bound) (:type observed) (:type after)))
       (is (= :not-attempted (:observation-status before)))
       (is (= :exact-typed-catalog-to-issued-certified-plan-match
              (:original-transition-source before)
              (:original-transition-source observed)))
       (is (re-matches #"sha256:[0-9a-f]{64}"
                       (:transition-fingerprint observed)))
       (is (= :bounded-same-invocation-adapter-correlation (:status observed)))
       (is (true? (:prepared-operations-agree? observed)))
       (is (true? (:prepared-options-agree? observed)))
       (is (true? (:definitive-execute-key-observed? observed)))
       (is (= :bounded-source-matched-adapter-plan-footprint
              (:declared-source-to-observed-adapter-footprint observed)
              (:declared-source-to-observed-adapter-footprint after)))
       (is (= :bounded-same-invocation-adapter-correlation
              (:observation-status after)))
       (is (= 1 @transactions))
       (is (= 1 (count @api-calls)))
       (is (= :not-established
              (:typed-to-certified-executor-transition-equivalence observed)))
       (is (= :not-established (:original-business-policy-authenticity observed)))
       (is (= :not-established (:typed-gateway-used-by-handler observed)))
       (is (= :not-established (:xtdb-api-and-receipt-authenticity observed)))
       (is (= :not-issued (:certificate observed)))
       (is (not-any? #(contains? observed %)
                     [:contract :source :catalog :executor :handler :plan
                      :request :response :tx-ops :witness :handle]))
       (is (= :aligned-observation-already-attempted
              (reason #((:observe-once! bound) (request)))))))))

(deftest differing-guard-or-assignment-cannot-match-original-source
  (let [c (aligned-catalog)]
    (doseq [changed [(assoc-in c
                              [:operations 0 :construction :guard :expression 1 2 1]
                              :claimed)
                     (assoc-in c
                               [:operations 0 :construction :replacements
                                :request/status]
                               [:literal :open])]]
      (is (= :typed-and-certified-original-transition-diverged
             (reason #(aligned-issuer changed)))))
    (is (empty? @(:calls *case*)))))

(deftest source-alignment-does-not-authenticate-weakened-business-rules
  (let [weak (assoc (aligned-catalog) :invariants
                    {:request/version-weak-bound
                     {:readers #{:request/version}
                      :expression [:< [:read :request/version]
                                   [:literal 1000000]]}})
        bound (aligned-issuer weak)
        r ((:summary! bound))]
    ;; The exact transition matches but declared invariants are chosen by the
    ;; caller; the matcher cannot know which business rules were omitted.
    (is (= :exact-typed-catalog-to-issued-certified-plan-match
           (:original-transition-source r)))
    (is (= :not-established (:original-business-policy-authenticity r)))
    (is (= :not-issued (:certificate r)))
    (is (empty? @(:calls *case*)))))

(deftest source-alignment-checker-swap-consumes-local-permit
  (let [bound (aligned-issuer)]
    (with-redefs [gesso.model.certified-plan/describe-plan-operation
                  (fn [_] {:transition-fingerprint :forged})]
      (is (= :substituted-original-source-checker
             (reason #((:observe-once! bound) (request))))))
    (is (= :aligned-observation-already-attempted
           (reason #((:observe-once! bound) (request)))))
    (is (empty? @(:calls *case*)))))


;; v1764: Source equality alone is never a physical-transaction observation.
;; The adapter's exact prepared transaction footprint must match as well.
(deftest aligned-source-does-not-promote-a-reconcile-only-execution
  (reset! (:mode *case*) :reconcile)
  (let [bound (aligned-issuer)
        before ((:summary! bound))
        result ((:observe-once! bound) (request))
        after ((:summary! bound))]
    (is (= :not-established
           (:declared-source-to-observed-adapter-footprint before)
           (:declared-source-to-observed-adapter-footprint result)
           (:declared-source-to-observed-adapter-footprint after)))
    (is (= :exact-typed-catalog-to-issued-certified-plan-match
           (:original-transition-source result)))
    (is (= :no-qualified-same-invocation-adapter-correlation (:status result)))
    (is (= :not-issued (:certificate result)))
    (is (= [:input :response] @(:calls *case*)))))

(deftest altered-physical-ops-options-and-nondefinitive-submits-are-not-promoted
  (doseq [[settings expected-status]
          [[{:change-ops #(conj % [:rogue :unaccounted])}
            :prepared-plan-diverges-from-adapter]
           [{:tx-options {:metadata {:unexpected :value}}}
            :prepared-options-diverge-from-adapter]
           [{:tx-mode :submit-tx}
            :submit-only-not-definitive]
           [{:status :rejected}
            :unconfirmed-or-uncertain-outcome]]]
    ((helper fixture-ns 'artificial-execution) settings
     (fn [{:keys [transactions api-calls]}]
       (let [bound (aligned-issuer)
             result ((:observe-once! bound) (request))]
         (is (= expected-status (:physical-observation-status result)))
         (is (= :no-qualified-same-invocation-adapter-correlation
                (:status result)))
         (is (= :not-established
                (:declared-source-to-observed-adapter-footprint result)
                (:declared-source-to-observed-adapter-footprint
                 ((:summary! bound)))))
         (is (= :not-established (:typed-gateway-used-by-handler result)))
         (is (= :not-issued (:certificate result)))
         (is (= 1 @transactions) "No retries to repair an observation")
         (is (= 1 (count @api-calls))))))))

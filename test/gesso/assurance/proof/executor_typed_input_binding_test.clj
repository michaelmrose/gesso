(ns gesso.assurance.proof.executor-typed-input-binding-test
  "v1385: Permanent adversarial regression for v1384's construction-site typed
   input projection. The original issued writer source and resolved-operation
   executor are exercised; only the outgoing XTDB call is stubbed. The
   construction observer can be independently withheld or corrupted in these
   tests. A matching projection is a local diagnostic, NOT authenticated
   request identity, physical submission, authorization, or a certificate."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [gesso.assurance.proof.executor-controlled-acquisition :as controlled]
            [gesso.assurance.proof.physical-execution-provenance-boundary-test :as fixture]
            [gesso.assurance.proof.typed-value-semantics :as typed]
            [gesso.model.command :as command]
            [gesso.model.command-resolution :as resolution]
            [gesso.model.resolved-operation :as resolved]
            [gesso.model.tx :as tx]))

;; The expensive issued source is created once. Tests never mint synthetic
;; certificates or replace the original application/source verifier.
(use-fixtures :once
  (first (:clojure.test/once-fixtures
          (meta (the-ns
                 'gesso.assurance.proof.physical-execution-provenance-boundary-test)))))

(def ^:private observation-keys
  [resolved/observation-context-key
   resolved/construction-input-context-key])

(def ^:private expected-open
  #{:typed-input-binding
    :transaction-internal-prestate
    :guard-authorization
    :assignment-write-footprint
    :frame-and-unmodeled-fields
    :serial-concurrency-validation
    :committed-outcome-incorporation
    :writer-provenance-and-deployment
    :universal-execution-refinement})

(defn- observed []
  (get-in fixture/*source* [:observed-source :transition-source :observation]))

(defn- source
  ([] (source (fn [_ _] nil)))
  ([on-callback]
   (let [o (observed)
         plan (:declared-plan o)
         commands (filterv #(not= :gesso_commands (first (command/target %)))
                           (:commands plan))
         record (:persisted-command-record o)]
     {:writer-source (:writer-source fixture/*source*)
      :operation-id :example/claim
      :binding (:command-binding o)
      :configuration
      {:operation :example/claim
       :read-record! (fn [ctx _] (on-callback :read ctx) nil)
       :may-disclose? (fn [ctx _ _] (on-callback :disclose ctx) true)
       :construct! (fn [ctx _]
                     (on-callback :construct ctx)
                     {:now (:gesso.command/created-at record)
                      :outcome (:gesso.command/outcome record)
                      :plan {:commands commands :emit false}})}})))

(defn- typed-source
  ([] (typed-source (source)))
  ([src] (assoc src :input-paths {:input/actor [:context :app/actor]})))

(defn- tx-commit [calls]
  (fn [_ _]
    (let [n (swap! calls inc)]
      {:commit/status :committed :tx-result {:tx-id n}})))

(defn- failure [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn- outcome [f]
  (let [data (failure f)]
    (or (:reason data) (:error/kind data))))

(defn- no-authority! [result]
  (is (= :executor-controlled-hook-diagnostic-only (:proof-status result)))
  (is (= :not-issued (:certificate result)))
  (is (= :not-admitted (:admission result)))
  (is (= :none (:authority result)))
  (is (= :none (:transaction-authority result)))
  (is (false? (:observer-origin-authenticated? result)))
  (is (false? (:physical-submission-authenticated? result)))
  (is (= expected-open
         (set (map :id (:physical-refinement-obligations result)))))
  (is (every? #(= :open (:status %))
              (:physical-refinement-obligations result)))
  (let [input (:typed-input-observation result)]
    (is (= :open (:physical-refinement-status input)))
    (is (= :not-issued (:certificate input)))
    (is (= :none (:authority input)))
    (is (= :not-established
           (:executed-request-to-typed-input-proof input)))
    (is (= :trusted-invocation-not-authenticated
           (:typed-input-value-provenance input)))))

(defn- no-observer-in-callbacks [seen]
  (fn [phase ctx]
    (swap! seen conj phase)
    (doseq [key observation-keys]
      (is (not (contains? ctx key))))))

(defn- publish-event-var []
  (or (ns-resolve 'gesso.model.resolved-operation
                  'publish-construction-input!)
      (throw (ex-info "Construction input event publisher was removed" {}))))

(defn- injected-event [ctx binding]
  {:scope resolved/construction-input-observation-scope
   :phase :construction-input
   :operation (:operation binding)
   :context ctx
   :binding binding})

(deftest matching-inputs-are-bounded-unprivileged-diagnostics
  (let [calls (atom 0) callbacks (atom [])
        src (typed-source (source (no-observer-in-callbacks callbacks)))]
    (with-redefs [tx/transact! (tx-commit calls)]
      (let [result (controlled/run-controlled-typed-submit!
                    src {:app/actor :user/a :app/marker :opaque})
            input (:typed-input-observation result)]
        (is (= 1 @calls))
        (is (= [:read :construct] @callbacks))
        (is (= :incorporated (get-in result [:resolution :status])))
        (is (= :complete-paired-hook-observation (:capture-status result)))
        (is (= 1 (:construction-input-events input)))
        (is (= :matched (:construction-site-correlation input)))
        (is (= #{:input/actor} (:declared-input-field-ids input)))
        (is (= 1 (:projected-field-count input)))
        (is (= :caller-supplied-closed-paths (:typed-projection-source input)))
        (is (= :one-process-local-construction-hook-only (:scope input)))
        (is (nil? (:app/actor input)))
        (is (not (some #(= :user/a %) (vals input))))
        (no-authority! result)))))

(deftest nested-map-and-vector-paths-preserve-typed-correlation
  (let [calls (atom 0)]
    (with-redefs [tx/transact! (tx-commit calls)]
      (doseq [[path ctx]
              [[[:context :app/envelope :actor]
                {:app/envelope {:actor :user/a}}]
               [[:context :app/envelopes 0 :actor]
                {:app/envelopes [{:actor :user/b}]}]]]
        (let [src (assoc (source) :input-paths {:input/actor path})
              result (controlled/run-controlled-typed-submit! src ctx)]
          (is (= :matched (get-in result
                                  [:typed-input-observation
                                   :construction-site-correlation])))
          (no-authority! result)))
      (is (= 2 @calls)))))


(deftest integer-path-segments-also-read-integer-map-keys
  ;; Path segments are generic persistent-map keys as well as vector indexes.
  ;; A successful projection must not be advertised as proof of the shape of
  ;; the application's original request.
  (let [calls (atom 0)
        src (assoc (source) :input-paths
                   {:input/actor [:context :app/data 0 :actor]})]
    (with-redefs [tx/transact! (tx-commit calls)]
      (let [result (controlled/run-controlled-typed-submit!
                    src {:app/data {0 {:actor :user/a}}})]
        (is (= 1 @calls))
        (is (= :matched (get-in result
                                [:typed-input-observation
                                 :construction-site-correlation])))
        (no-authority! result)))))

(deftest type-correct-change-of-invocation-input-is-not-principal-proof
  ;; Different well-typed inputs produce distinct local observations, but
  ;; neither observation proves a browser request or authenticated principal.
  (let [calls (atom 0)]
    (with-redefs [tx/transact! (tx-commit calls)]
      (let [a (controlled/run-controlled-typed-submit!
               (typed-source) {:app/actor :user/a})
            b (controlled/run-controlled-typed-submit!
               (typed-source) {:app/actor :user/b})]
        (is (= 2 @calls))
        (is (= :matched (get-in a [:typed-input-observation
                                   :construction-site-correlation])))
        (is (= :matched (get-in b [:typed-input-observation
                                   :construction-site-correlation])))
        (is (not= (get-in a [:acquisition-report :attempt-id])
                  (get-in b [:acquisition-report :attempt-id])))
        (no-authority! a)
        (no-authority! b)))))

(deftest type-compatible-but-semantically-wrong-path-does-not-become-proof
  ;; The application controls the projection mapping. A qualified operation
  ;; keyword happens to type-check as an actor: Gesso does NOT certify that
  ;; the mapping represents the authenticated actor.
  (let [calls (atom 0)
        src (assoc (source) :input-paths
                   {:input/actor [:binding :operation]})]
    (with-redefs [tx/transact! (tx-commit calls)]
      (let [result (controlled/run-controlled-typed-submit! src {})]
        (is (= 1 @calls))
        (is (= :matched (get-in result [:typed-input-observation
                                        :construction-site-correlation])))
        (is (= :caller-supplied-closed-paths
               (get-in result [:typed-input-observation
                               :typed-projection-source])))
        (no-authority! result)))))

(deftest missing-null-and-wrongly-typed-values-fail-before-callbacks
  (let [calls (atom 0) callbacks (atom [])
        src (typed-source (source (no-observer-in-callbacks callbacks)))]
    (with-redefs [tx/transact! (tx-commit calls)]
      (doseq [ctx [{} {:app/actor nil} {:app/actor false}
                   {:app/actor 12} {:app/actor "user/a"}
                   {:app/actor [:user/a]} {:app/actor {:name :user/a}}
                   {:app/actor (java.util.Date.)}]]
        (is (= :ill-typed-or-missing-input
               (outcome #(controlled/run-controlled-typed-submit! src ctx)))))
      (is (= [] @callbacks))
      (is (= 0 @calls)))))

(deftest missing-and-wrongly-structured-nested-inputs-are-absent
  (let [calls (atom 0) callbacks (atom [])
        src (assoc (source (no-observer-in-callbacks callbacks))
                   :input-paths {:input/actor [:context :app/data 0 :actor]})]
    (with-redefs [tx/transact! (tx-commit calls)]
      (doseq [ctx [{} {:app/data nil} {:app/data []}
                   {:app/data [:user/a]}
                   {:app/data [{}]}
                   {:app/data [{:actor nil}]}]]
        (is (= :ill-typed-or-missing-input
               (outcome #(controlled/run-controlled-typed-submit! src ctx)))))
      (is (zero? @calls))
      (is (empty? @callbacks)))))

(deftest closed-projection-refuses-missing-extra-or-metadata-bearing-shapes
  (let [calls (atom 0) callbacks (atom [])
        src (source (no-observer-in-callbacks callbacks))]
    (with-redefs [tx/transact! (tx-commit calls)]
      (doseq [paths [nil {}
                     {:input/actor [:context :app/actor]
                      :input/extra [:context :app/extra]}
                     (with-meta {:input/actor [:context :app/actor]}
                       {:claimed :proof})
                     (doto (java.util.HashMap.)
                       (.put :input/actor [:context :app/actor]))]]
        (is (= :incomplete-input-projection
               (outcome #(controlled/run-controlled-typed-submit!
                          (assoc src :input-paths paths)
                          {:app/actor :user/a})))))
      (is (zero? @calls))
      (is (empty? @callbacks)))))

(deftest invalid-path-tokens-and-depth-fail-before-application-execution
  (let [calls (atom 0) callbacks (atom [])
        src (source (no-observer-in-callbacks callbacks))
        illegal [[:ctx :app/actor]
                 [:context]
                 [:context :app/actor -1]
                 [:context :app/actor 257]
                 [:context :app/actor "key"]
                 [:context :app/actor nil]
                 [:context :app/actor []]
                 [:context resolved/observation-context-key]
                 [:context resolved/construction-input-context-key]
                 [:context :a :b :c :d :e :f :g :h]
                 (with-meta [:context :app/actor] {:claim :proof})
                 '(:context :app/actor)]]
    (with-redefs [tx/transact! (tx-commit calls)]
      (doseq [path illegal]
        (is (= :invalid-input-path
               (outcome #(controlled/run-controlled-typed-submit!
                          (assoc src :input-paths {:input/actor path})
                          {:app/actor :user/a})))))
      (is (= 0 @calls))
      (is (empty? @callbacks)))))

(deftest context-observer-injection-is-rejected-even-with-valid-inputs
  (let [calls (atom 0) callbacks (atom [])
        src (typed-source (source (no-observer-in-callbacks callbacks)))]
    (with-redefs [tx/transact! (tx-commit calls)]
      (doseq [hook observation-keys
              forged [nil (fn [& _] nil) :supplied-observer]]
        (is (= :preexisting-observation-hook
               (outcome #(controlled/run-controlled-typed-submit!
                          src {:app/actor :user/a hook forged})))))
      (is (zero? @calls))
      (is (empty? @callbacks)))))

(deftest wrong-operation-identities-fail-before-callbacks
  (let [calls (atom 0) callbacks (atom [])
        src (typed-source (source (no-observer-in-callbacks callbacks)))]
    (with-redefs [tx/transact! (tx-commit calls)]
      (doseq [bad [(assoc src :operation-id :example/other)
                   (assoc-in src [:configuration :operation] :example/other)
                   (assoc-in src [:binding :operation] :example/other)]]
        (is (= :operation-source-mismatch
               (outcome #(controlled/run-controlled-typed-submit!
                          bad {:app/actor :user/a})))))
      (is (zero? @calls))
      (is (empty? @callbacks)))))

(deftest altered-principal-and-command-identities-are-not-silent-proof
  (let [calls (atom 0) callbacks (atom [])
        src (typed-source (source (no-observer-in-callbacks callbacks)))
        binding (:binding src)]
    (with-redefs [tx/transact! (tx-commit calls)]
      (doseq [bad-binding [(assoc binding :principal :forged)
                           (assoc binding :command-id :forged)
                           (assoc binding :arguments "not-portable")]]
        (is (= :invalid-binding
               (outcome #(controlled/run-controlled-typed-submit!
                          (assoc src :binding bad-binding)
                          {:app/actor :user/a})))))
      (is (zero? @calls))
      (is (empty? @callbacks)))))

(deftest preexisting-source-observer-and-fake-type-schema-are-refused
  (let [calls (atom 0) callbacks (atom [])
        src (typed-source (source (no-observer-in-callbacks callbacks)))]
    (with-redefs [tx/transact! (tx-commit calls)]
      (doseq [extra [:observe! :complete! :observer :input-types
                     :typed-input-observation :acquisition-report]]
        (is (= :invalid-source-shape
               (outcome #(controlled/run-controlled-typed-submit!
                          (assoc src extra {}) {:app/actor :user/a})))))
      (is (= :invalid-source-shape
             (outcome #(controlled/run-controlled-typed-submit!
                       (with-meta src {:trusted true}) {:app/actor :user/a}))))
      (is (zero? @calls))
      (is (empty? @callbacks)))))

(deftest durable-replay-exposes-missing-construction-event
  (let [calls (atom 0)
        durable (:persisted-command-record (observed))
        src (assoc-in (typed-source) [:configuration :read-record!]
                      (fn [_ _] durable))]
    (with-redefs [tx/transact! (tx-commit calls)]
      (let [result (controlled/run-controlled-typed-submit!
                    src {:app/actor :user/a})
            input (:typed-input-observation result)]
        (is (= 0 @calls))
        (is (= 0 (:construction-input-events input)))
        (is (= :missing (:construction-site-correlation input)))
        (is (= :no-complete-evidence (:capture-status result)))
        (no-authority! result)))))

(deftest rejected-transaction-may-have-matched-inputs-without-commit
  (let [calls (atom 0)]
    (with-redefs [tx/transact!
                  (fn [& _]
                    (swap! calls inc)
                    (throw (ex-info "rejected" {:xtdb.error/code :xtdb/assert-failed})))]
      (let [result (controlled/run-controlled-typed-submit!
                    (typed-source) {:app/actor :user/a})]
        (is (= 1 @calls))
        (is (= :matched (get-in result
                                [:typed-input-observation
                                 :construction-site-correlation])))
        (is (= :rejected (get-in result
                                 [:acquisition-report :captured-physical-status])))
        (is (not= :incorporated (get-in result [:resolution :status])))
        (no-authority! result)))))

(deftest indeterminate-tx-result-does-not-borrow-construction-proof
  (let [calls (atom 0)]
    (with-redefs [tx/transact!
                  (fn [& _]
                    (swap! calls inc)
                    {:commit/status :not-committed})]
      (let [result (controlled/run-controlled-typed-submit!
                    (typed-source) {:app/actor :user/a})]
        (is (= 1 @calls))
        (is (= :matched (get-in result
                                [:typed-input-observation
                                 :construction-site-correlation])))
        (is (= :no-complete-evidence (:capture-status result)))
        (is (= :nondefinitive-physical-outcome (:capture-reason result)))
        (no-authority! result)))))

(deftest missing-construction-event-is-diagnostic-not-a-veto
  (let [calls (atom 0)]
    (with-redefs-fn {(publish-event-var) (fn [_ _ _] nil)
                    #'tx/transact! (tx-commit calls)}
      (fn []
        (let [result (controlled/run-controlled-typed-submit!
                      (typed-source) {:app/actor :user/a})]
          (is (= 1 @calls))
          (is (= :incorporated (get-in result [:resolution :status])))
          (is (= 0 (get-in result
                           [:typed-input-observation :construction-input-events])))
          (is (= :missing (get-in result
                                  [:typed-input-observation
                                   :construction-site-correlation])))
          (no-authority! result))))))

(deftest duplicate-construction-event-is-invalid-not-two-witnesses
  (let [calls (atom 0)]
    (with-redefs-fn {(publish-event-var)
                    (fn [observer ctx binding]
                      (observer (injected-event ctx binding))
                      (observer (injected-event ctx binding)))
                    #'tx/transact! (tx-commit calls)}
      (fn []
        (let [result (controlled/run-controlled-typed-submit!
                      (typed-source) {:app/actor :user/a})]
          (is (= 1 @calls))
          (is (= :incorporated (get-in result [:resolution :status])))
          (is (= 2 (get-in result
                           [:typed-input-observation :construction-input-events])))
          (is (= :invalid (get-in result
                                  [:typed-input-observation
                                   :construction-site-correlation])))
          (no-authority! result))))))

(deftest altered-construction-context-and-binding-are-invalid
  (let [calls (atom 0)]
    (with-redefs-fn {(publish-event-var)
                    (fn [observer ctx binding]
                      (observer (assoc (injected-event ctx binding)
                                       :context (assoc ctx :app/actor :user/forged))))
                    #'tx/transact! (tx-commit calls)}
      (fn []
        (let [result (controlled/run-controlled-typed-submit!
                      (typed-source) {:app/actor :user/a})]
          (is (= :invalid (get-in result
                                  [:typed-input-observation
                                   :construction-site-correlation])))
          (is (= 1 @calls))
          (no-authority! result))))
    (with-redefs-fn {(publish-event-var)
                    (fn [observer ctx binding]
                      (observer (assoc (injected-event ctx binding)
                                       :binding (assoc binding :arguments {:forged true}))))
                    #'tx/transact! (tx-commit calls)}
      (fn []
        (let [result (controlled/run-controlled-typed-submit!
                      (typed-source) {:app/actor :user/a})]
          (is (= :invalid (get-in result
                                  [:typed-input-observation
                                   :construction-site-correlation])))
          (is (= 2 @calls))
          (no-authority! result))))))

(deftest altered-construction-operation-and-added-field-are-invalid
  (let [calls (atom 0)]
    (doseq [alter [(fn [event] (assoc event :operation :example/forged))
                   (fn [event] (assoc event :claim :authenticated))
                   (fn [event] (dissoc event :phase))]]
      (with-redefs-fn {(publish-event-var)
                      (fn [observer ctx binding]
                        (observer (alter (injected-event ctx binding))))
                      #'tx/transact! (tx-commit calls)}
        (fn []
          (let [result (controlled/run-controlled-typed-submit!
                        (typed-source) {:app/actor :user/a})]
            (is (= :invalid (get-in result
                                    [:typed-input-observation
                                     :construction-site-correlation])))
            (no-authority! result)))))
    (is (= 3 @calls))))

(deftest substituted-typed-interpreter-cannot-certify-execution
  (let [calls (atom 0) callbacks (atom [])
        src (typed-source (source (no-observer-in-callbacks callbacks)))]
    (with-redefs [tx/transact! (tx-commit calls)
                  typed/observe-state! (fn [& _] {:forged :accept})]
      ;; A source-recheck may reject the substitution even earlier than the
      ;; controlled boundary. Both fail before callbacks or submission.
      (is (#{:substituted-typed-input-interpreter :read-absent-reader}
           (outcome #(controlled/run-controlled-typed-submit!
                     src {:app/actor :user/a}))))
      (is (= 0 @calls))
      (is (empty? @callbacks)))))

(deftest invalid-input-errors-are-redacted
  (let [src (typed-source)
        error (failure #(controlled/run-controlled-typed-submit!
                         src {:app/actor "secret-actor-token"}))]
    (is (= :ill-typed-or-missing-input (:reason error)))
    (is (= #{:executor-controlled-acquisition/rejection :reason :at}
           (set (keys error))))
    (is (not (.contains (pr-str error) "secret-actor-token")))))

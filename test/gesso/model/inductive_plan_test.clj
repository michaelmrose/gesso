(ns gesso.model.inductive-plan-test
  "v1291: permanent adversarial coverage of optional v1290 induction-bound
   authoritative plan issuance. Exercise original source-owned induction,
   v1251 plan registration, physical candidate validation, and identity-based
   freshness together. No physical commit is simulated as mathematical proof;
   intercept only the final durable transaction call when testing submission.

   An issued function is still not a proof of physical refinement, complete
   production writer coverage, authorization, atomicity or deployed adoption."
  (:require [clojure.set :as set]
            [clojure.test :refer [deftest is use-fixtures]]
            [gesso.assurance.dependency :as dependency]
            [gesso.assurance.inductive-report :as inductive]
            [gesso.assurance.proof.certificate :as certificate]
            [gesso.assurance.proof.induction :as induction]
            [gesso.assurance.proof.induction-dependency :as closure]
            [gesso.model.certified-plan :as certified]
            [gesso.model.certified-plan-test]
            [gesso.model.inductive-plan :as plan]
            [gesso.model.tx :as tx]))

(def ^:private operation :example/claim)
(def ^:private plan-error :gesso.model.inductive-plan/invalid-inductive-plan)
(def ^:private certified-error :gesso.model.certified-plan/invalid-certified-plan)
(def ^:private status-domain
  {:readers #{:request/status}
   :expression [:in [:read :request/status]
                [:literal #{:open :claimed}]]})
(def ^:private premise
  :gesso.assurance.proof.induction/matching-declared-abstract-machine)
(def ^:private external-conditions
  #{:production-transition-correspondence
    :current-runtime-authorization
    :atomic-transaction-enforcement
    :checker-meta-soundness})

(defn- private-fixture [name]
  (or (some-> (ns-resolve 'gesso.model.certified-plan-test name) deref)
      (throw (ex-info "Missing v1252 source-owning test fixture" {:name name}))))

(defn- catalog-for [construction]
  {:id :example/induction-bound-claims
   :declared-operation-ids #{operation}
   :initial-condition status-domain
   :invariants {:example/valid-status status-domain}
   :operations [{:id operation :construction construction}]})

(defn- proof-package [catalog]
  (let [graph (closure/build-induction-dependency-graph! catalog)
        proof (induction/check-induction! catalog)
        dependency-handle (closure/check-induction-dependencies! catalog graph)
        certificate-handle
        (certificate/admit-certificate!
         {:proof-kind :conditional-induction
          :proof-source catalog
          :graph-handle (dependency/check-graph! graph)
          :current-graph graph
          :expected-open-premises #{premise}
          :expected-open-obligations
          (set/union external-conditions
                     (disj (:open-meta-obligations proof)
                           :certificate-admission))})]
    {:catalog catalog
     :certificate-handle certificate-handle
     :dependency-handle dependency-handle
     :current-graph graph}))

(def ^:dynamic *env* nil)
(def ^:dynamic *source* nil)
(def ^:dynamic *issued* nil)

(use-fixtures
  :once
  (fn [run]
    ;; The v1252 fixture owns the original guarded construction and all
    ;; acquisition/command callbacks. No duplicate implementation is invented.
    (let [env ((private-fixture 'environment))
          catalog (catalog-for (get-in env [:source :construction]))
          induction-source (proof-package catalog)
          source {:inductive-source induction-source
                  :certified-plan-source (:source env)}
          issued (plan/build-inductive-plan-operation! source)]
      (binding [*env* env *source* source *issued* issued]
        (run)))))

(defn- error [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn- reject! [f expected-type expected-reason]
  (let [data (error f)]
    (is (map? data) (str "Missing typed failure: " expected-reason))
    (is (= expected-type (:error/type data)) (pr-str data))
    (is (= expected-reason (:reason data)) (pr-str data))
    data))

(defn- reject-plan! [reason f]
  (reject! f plan-error reason))

(defn- check-current
  ([] (plan/require-current-inductive-plan! *issued* *source*))
  ([source] (plan/require-current-inductive-plan! *issued* source)))

(defn- issued-report []
  (plan/describe-inductive-plan *issued*))

(defn- no-physical-claims! [r]
  (is (= plan/report-type (:type r)))
  (is (= plan/adapter-version (:version r)))
  (is (= operation (:operation r)))
  (is (= #{operation} (:catalog-operation-ids r)))
  (is (= #{:example/valid-status} (:invariant-ids r)))
  (is (= :exact-guarded-transition-construction (:source-join r)))
  (is (= :original-v1251-pre-submit-checker (:plan-gate r)))
  (is (= :conditionally-checked-in-declared-abstract-machine
         (:inductive-invariant r)))
  (is (= :trusted-observation-only (:prestate-assurance r)))
  (doseq [k [:runtime-induction-gating :actual-router-adoption
             :atomicity-assurance :physical-correspondence
             :physical-write-path-completeness]]
    (is (= :not-established (get r k)) (str k)))
  (is (= :none (:transaction-authority r)))
  (is (= :none (:authority r)))
  (is (not (contains? r :valid?)))
  (is (not (contains? r :proved?)))
  (is (= #{operation} (set (keys (:original-open-obligations r)))))
  (is (seq (get (:original-open-obligations r) operation)))
  (doseq [premise [:authorization-and-callback-purity
                   :prestate-to-commit-atomic-guard-enforcement
                   :physical-write-path-completeness
                   :abstract-to-xtdb-transition-correspondence
                   :deployed-handler-and-router-adoption
                   :induction-checker-independent-soundness]]
    (is (contains? (:open-obligations r) premise) (str premise)))
  (is (seq (:open-premises r)))
  r)

(deftest current-issued-plan-has-conditional-only-report
  (is (fn? *issued*))
  (is (plan/inductive-plan-operation? *issued*))
  (is (certified/checked-plan-operation? *issued*))
  (let [r (no-physical-claims! (issued-report))]
    (is (= r (check-current)))
    (is (= r (check-current (into {} *source*))))
    (is (= (:transition-fingerprint r)
           (:transition-fingerprint (certified/describe-plan-operation *issued*))))
    (is (re-matches #"sha256:[0-9a-f]{64}"
                    (:subject-fingerprint r)))))

(deftest only-issued-function-identity-qualifies
  (let [ordinary (certified/build-certified-plan-operation!
                  (:certified-plan-source *source*))
        forged (with-meta (fn [& _] {:status :incorporated}) (meta *issued*))]
    (is (certified/checked-plan-operation? ordinary))
    (is (false? (plan/inductive-plan-operation? ordinary)))
    (is (false? (plan/inductive-plan-operation? forged)))
    (is (false? (plan/inductive-plan-operation? (issued-report))))
    (is (false? (plan/inductive-plan-operation? nil)))
    (reject-plan! :unregistered-inductive-plan
                  #(plan/describe-inductive-plan forged))
    (reject-plan! :unregistered-inductive-plan
                  #(plan/require-current-inductive-plan! ordinary *source*))
    (reject-plan! :unregistered-inductive-plan
                  #(plan/require-current-inductive-plan! (issued-report) *source*))
    (is (= :none (:authority (issued-report))))))

(deftest current-source-shape-is-strict
  (doseq [source [(dissoc *source* :inductive-source)
                  (assoc *source* :valid? true)
                  (with-meta *source* {:proved? true})
                  []
                  nil]]
    (is (= plan-error (:error/type (error #(check-current source))))))
  (doseq [bad [(assoc-in *source* [:inductive-source :certificate-handle] nil)
               (assoc-in *source* [:inductive-source :dependency-handle] nil)
               (assoc-in *source* [:inductive-source :extra] :forged)
               (with-meta (:inductive-source *source*) {:admitted true})]]
    ;; The last case supplies an inner map; others already supply the top map.
    (let [current (if (contains? bad :inductive-source)
                    bad (assoc *source* :inductive-source bad))]
      (is (= plan-error (:error/type (error #(check-current current)))))))
  (is (not= :runtime-authorized (:authority (issued-report)))))

(deftest source-change-invalidates-current-plan
  (let [candidates
        [(assoc-in *source* [:inductive-source :catalog :invariants
                             :example/valid-status :expression]
                   [:literal true])
         (assoc-in *source* [:inductive-source :catalog :initial-condition
                             :expression]
                   [:literal true])
         (assoc-in *source* [:inductive-source :catalog :declared-operation-ids]
                   #{:example/other})
         (assoc-in *source* [:inductive-source :catalog :operations 0
                             :construction :guard :expression]
                   [:literal true])
         (assoc-in *source* [:inductive-source :current-graph :roots]
                   #{:forged/root})]]
    (doseq [changed candidates]
      (reject-plan! :changed-induction-registration
                    #(check-current changed)))))

(deftest changed-live-handles-are-not-fresh
  (doseq [key [:certificate-handle :dependency-handle]]
    (reject-plan! :changed-induction-registration
                  #(check-current
                    (assoc-in *source* [:inductive-source key]
                              {:copied-description true}))))
  (is (= (issued-report) (check-current))))

(deftest plan-source-changes-are-rejected
  (doseq [k [:current-document! :transition-inputs! :target!]]
    (let [old (get-in *source* [:certified-plan-source k])
          changed (assoc-in *source* [:certified-plan-source k]
                            (fn [& args] (apply old args)))]
      (is (= certified-error
             (:error/type (error #(check-current changed)))))))
  (doseq [changed
          [(assoc-in *source* [:certified-plan-source :command-operation] :cancel)
           (assoc-in *source* [:certified-plan-source :field-map :request/score]
                     :physical/score)
           (assoc-in *source* [:certified-plan-source :construction :replacements
                               :request/status]
                     [:literal :open])
           (assoc-in *source* [:certified-plan-source :certified-source
                               :resolved-configuration :construct!]
                     (fn [_ _] {:now :forged}))]]
    (is (contains? #{plan-error certified-error}
                   (:error/type (error #(check-current changed)))))))

(deftest same-operation-id-with-different-guard-cannot-be-issued
  (let [changed (assoc-in *source* [:certified-plan-source :construction
                                    :guard :expression]
                          [:literal true])]
    (reject-plan! :plan-abstract-construction-mismatch
                  #(plan/build-inductive-plan-operation! changed)))
  (let [changed (assoc-in *source* [:certified-plan-source :construction
                                    :replacements :request/status]
                          [:literal :open])]
    (reject-plan! :plan-abstract-construction-mismatch
                  #(plan/build-inductive-plan-operation! changed))))

(deftest incorrect-registration-cannot-issue-plan
  (let [changed (assoc-in *source* [:certified-plan-source :certified-source
                                    :operation]
                          :example/other)]
    (reject-plan! :registered-plan-transition-mismatch
                  #(plan/build-inductive-plan-operation! changed)))
  (let [changed (assoc-in *source* [:certified-plan-source :certified-source
                                    :proof-source :transition :guard]
                          {:readers #{} :expression [:literal true]})]
    (is (map? (error #(plan/build-inductive-plan-operation! changed)))))
  (is (= (issued-report) (check-current))))

(deftest forged-outward-checkers-cannot-issue-plan
  (let [base inductive/build-inductive-report!]
    (with-redefs [inductive/build-inductive-report!
                  (fn [_] (assoc (base (:inductive-source *source*))
                                 :authority :runtime-authorized))]
      (let [executor (plan/build-inductive-plan-operation! *source*)]
        (is (plan/inductive-plan-operation? executor))
        (is (= :none (:authority (plan/describe-inductive-plan executor))))))
    (with-redefs [certified/build-certified-plan-operation!
                  (fn [_] (throw (ex-info "forged public builder" {})))]
      (let [executor (plan/build-inductive-plan-operation! *source*)]
        (is (plan/inductive-plan-operation? executor))
        (is (= :none (:transaction-authority
                     (plan/describe-inductive-plan executor))))))))

(deftest issued-executor-remains-reconcile-only-without-explicit-submit
  (let [invocation (private-fixture 'invocation)
        calls (:calls *env*)
        before @calls]
    (is (= :unresolved (:status (*issued* {} (invocation :reconcile)))))
    (is (= before @calls))
    (is (nil? (error #(check-current))))))

(deftest invalid-physical-writes-never-reach-transaction
  (let [invocation (private-fixture 'invocation)
        command (private-fixture 'command)
        before (private-fixture 'before)
        after (private-fixture 'after)
        proposed (:proposed *env*)
        initial @proposed
        submissions (atom [])]
    (try
      (with-redefs [tx/transact! (fn [& args]
                                   (swap! submissions conj args)
                                   (throw (ex-info "UNEXPECTED SUBMISSION" {})))]
        (doseq [[bad reason]
                [[(assoc command :model/after
                         (assoc after :request/status :open))
                  :declared-write-mismatch]
                 [(assoc command :model/after
                         (assoc after :request/score 99))
                  :frame-preservation-failed]
                 [(assoc command :model/after
                         (assoc after :request/extra :secret))
                  :physical-field-footprint-mismatch]
                 [(assoc-in command [:model/expected :model/checks]
                            [[:request/version 123]])
                  :stale-update-check]]]
          (reset! proposed bad)
          (reject! #(*issued* {} (invocation :submit)) certified-error reason)))
      (finally (reset! proposed initial)))
    (is (empty? @submissions))
    (is (= (issued-report) (check-current)))))

(deftest valid-physical-candidate-uses-original-single-submit
  (let [invocation (private-fixture 'invocation)
        proposed (:proposed *env*)
        original @proposed
        submissions (atom [])]
    (try
      (with-redefs [tx/transact! (fn [_ctx plan]
                                   (swap! submissions conj plan)
                                   {:commit/status :committed
                                    :tx-result :test-only-simulated-commit})]
        (let [outcome (*issued* {} (invocation :submit))]
          (is (= :incorporated (:status outcome)))
          (is (= 1 (count @submissions)))
          (let [commands (:commands (first @submissions))]
            (is (= 2 (count commands)))
            (is (= #{[:requests 10] [:gesso_commands "v1252-claim"]}
                   (set (map (juxt :model/entity-type :model/id) commands)))))))
      (finally (reset! proposed original)))
    (is (= (issued-report) (check-current)))))

(deftest copied-descriptions-are-never-freshness-tokens
  (let [r (issued-report)]
    (is (= r (into {} r)))
    (is (false? (plan/inductive-plan-operation? r)))
    (reject-plan! :wrong-source-keys
                  #(check-current (assoc *source* :valid? true)))
    (reject-plan! :unregistered-inductive-plan
                  #(plan/require-current-inductive-plan! r *source*))))

(deftest open-assumptions-stay-open-after-actual-submission-test
  (let [r (issued-report)]
    (no-physical-claims! r)
    (is (= :not-established (:physical-correspondence r)))
    (is (= :not-established (:physical-write-path-completeness r)))
    (is (= :none (:transaction-authority r)))))

(deftest stale-trusted-current-document-is-rejected-before-submit
  ;; An induction-bound executor must continue using the v1251 candidate
  ;; gate. A caller-provided observation of a newer record is not evidence
  ;; that the original command still matches the acquired pre-state.
  (let [invocation (private-fixture 'invocation)
        original-before (private-fixture 'before)
        submissions (atom [])]
    (doseq [acquired [(assoc original-before :request/status :claimed)
                      (assoc original-before :request/version 99)]]
      (let [source (assoc-in *source*
                             [:certified-plan-source :current-document!]
                             (fn [_ _] acquired))
            executor (plan/build-inductive-plan-operation! source)]
        (with-redefs [tx/transact! (fn [& args]
                                     (swap! submissions conj args)
                                     (throw (ex-info "Unexpected write" {})))]
          (reject! #(executor {} (invocation :submit)) certified-error
                   :stale-or-inconsistent-prestate))
        (is (plan/inductive-plan-operation? executor))
        (is (= :none (:authority (plan/describe-inductive-plan executor))))))
    (is (empty? @submissions))))

(deftest plan-key-and-induction-key-injection-fail-closed
  (let [proposals
        [(assoc-in *source* [:certified-plan-source :admitted] true)
         (with-meta (:certified-plan-source *source*) {:proof true})
         (assoc-in *source* [:inductive-source :catalog :proved?] true)]]
    (doseq [candidate proposals]
      (let [input (if (contains? candidate :inductive-source)
                    candidate
                    (assoc *source* :certified-plan-source candidate))]
        (is (map? (error #(plan/build-inductive-plan-operation! input))))))
    (is (= (issued-report) (check-current)))))

(deftest modified-catalog-with-original-live-handles-never-issues-plan
  ;; Re-labeling the operation without changing the proof handles must not
  ;; install a new executor even if the v1251 plan remains unchanged.
  (doseq [changed [(assoc-in *source* [:inductive-source :catalog :id]
                             :example/other-catalog)
                   (assoc-in *source* [:inductive-source :catalog :operations 0
                                       :construction :replacements :request/status]
                             [:literal :open])
                   (assoc-in *source* [:inductive-source :catalog :invariants
                                       :example/valid-status :expression]
                             [:literal true])]]
    (is (map? (error #(plan/build-inductive-plan-operation! changed)))))
  (is (= (issued-report) (check-current))))

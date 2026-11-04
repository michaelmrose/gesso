(ns gesso.assurance.proof.certificate-test
  "v1238: permanent adversarial regressions for v1237's source-relative,
   independently rechecked abstract certificate admission.

   Successful admission is deliberately narrow: a live, JVM-local handle for
   one checked proof against one declared dependency graph. Neither the handle
   nor a copied report authorizes execution, proves the original Clojure
   function, discharges abstract premises, or certifies XTDB/CLJS behavior.
   Every negative assertion checks the rejection family and specific reason;
   positive cases ensure a checker that rejects everything cannot pass."
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.assurance.admission :as admission]
            [gesso.assurance.dependency :as dependency]
            [gesso.assurance.proof.certificate :as cert]
            [gesso.assurance.proof.derivation :as derivation]
            [gesso.assurance.proof.kernel :as kernel]
            [gesso.assurance.subject :as subject]))

(def ^:private request-subject
  {:kind :operation :id :example/claim
   :interface {:operation :example/claim}})

(def ^:private transition-source
  {:transition
   {:subject request-subject
    :state-types
    {:request/status {:type :enum :name :request/status
                      :values #{:open :claimed}}
     :request/owner {:type :keyword :nullable? true}
     :request/version {:type :integer}
     :request/note {:type :string :optional? true :nullable? true}}
    :input-types {:input/actor {:type :keyword}}
    :guard {:readers #{:request/status}
            :expression [:= [:read :request/status] [:literal :open]]}
    :reads #{:request/status :input/actor}
    :writes #{:request/status :request/owner}
    :assignments {:request/status [:literal :claimed]
                  :request/owner [:read :input/actor]}}
   :steps [{:id :step/guard :rule :guard :conclusion [:guard]}
           {:id :step/status :rule :assignment :field :request/status
            :conclusion [:eq [:post :request/status] [:literal :claimed]]}
           {:id :step/owner :rule :assignment :field :request/owner
            :conclusion [:eq [:post :request/owner] [:input :input/actor]]}
           {:id :step/both :rule :conjunction-introduction
            :from [:step/guard :step/status :step/owner]
            :conclusion [:and [:guard]
                         [:eq [:post :request/status] [:literal :claimed]]
                         [:eq [:post :request/owner] [:input :input/actor]]]}]
   :goal [:and [:guard]
          [:eq [:post :request/status] [:literal :claimed]]
          [:eq [:post :request/owner] [:input :input/actor]]]})

(def ^:private theorem-subject
  {:kind :rule :id :example/reflexive-theorem
   :interface {:theorem :example/reflexive}})

(def ^:private proposition-source
  {:subject theorem-subject
   :variables {}
   :premises {}
   :steps [{:id :step/reflex :rule :implication-reflexivity
            :conclusion [:implies [:true] [:true]]}]
   :goal [:implies [:true] [:true]]})

(def ^:private proposition-with-premise
  {:subject theorem-subject
   :variables {:result/score {:type :integer}}
   :premises
   {:premise/score
    [:atom {:readers #{:result/score}
            :expression [:> [:read :result/score] [:literal 7]]}]}
   :steps [{:id :step/score :rule :premise :premise :premise/score
            :conclusion
            [:atom {:readers #{:result/score}
                    :expression [:> [:read :result/score] [:literal 7]]}]}]
   :goal [:atom {:readers #{:result/score}
                 :expression [:> [:read :result/score] [:literal 7]]}]})

(def ^:private transition-premises
  #{:gesso.assurance.proof.derivation/abstract-relation-match})
(def ^:private transition-obligations
  #{:production-transition-correspondence
    :current-runtime-authorization
    :atomic-transaction-enforcement
    :checker-meta-soundness})
(def ^:private proposition-obligations
  #{:implementation-correspondence
    :current-runtime-authorization
    :checker-meta-soundness})

(defn- raw-graph
  "Each graph is a source declaration with live admitted subject handles.
   Optional dependencies are included in the root's declared footprint."
  ([root] (raw-graph root []))
  ([root dependencies]
   (let [deps (mapv (fn [s]
                      {:id (:id s)
                       :fingerprint (:fingerprint (subject/describe-subject s))})
                    dependencies)]
     {:roots #{(:id root)}
      :nodes (into
              [{:id (:id root) :subject root
                :subject-handle (admission/admit-subject! root)
                :depends-on deps}]
              (mapv (fn [s]
                      {:id (:id s) :subject s
                       :subject-handle (admission/admit-subject! s)
                       :depends-on []})
                    dependencies))})))

(defn- package
  ([kind source] (package kind source (raw-graph
                                       (if (= kind :guarded-transition)
                                         (get-in source [:transition :subject])
                                         (:subject source)))))
  ([kind source graph]
   {:proof-kind kind :proof-source source
    :graph-handle (dependency/check-graph! graph)
    :current-graph graph
    :expected-open-premises
    (if (= kind :guarded-transition)
      transition-premises
      (:open-premises (kernel/check-derivation! source)))
    :expected-open-obligations
    (if (= kind :guarded-transition)
      transition-obligations proposition-obligations)}))

(defn- exception-data
  [thunk]
  (try
    (thunk)
    nil
    (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn- rejects-as!
  "Require exact typed-error family and reason, not any ExceptionInfo."
  [thunk type reason]
  (let [data (exception-data thunk)]
    (is (= type (:error/type data)) (pr-str data))
    (is (= reason (:reason data)) (pr-str data))
    (when (= type :gesso.assurance.proof.certificate/invalid-certificate)
      (is (vector? (:at data)) (pr-str data)))
    data))

(defn- reject-certificate! [thunk reason]
  (rejects-as! thunk
               :gesso.assurance.proof.certificate/invalid-certificate
               reason))

(defn- checked-transition-report! [handle]
  (let [report (cert/describe-certificate handle)]
    (is (cert/checked-certificate? handle))
    (is (= cert/certificate-type (:type report)))
    (is (= cert/certificate-version (:version report)))
    (is (= :guarded-transition (:proof-kind report)))
    (is (= :matching-abstract-transition-only (:scope report)))
    (is (= :source-relative-abstract-proof-only (:certificate-scope report)))
    (is (= :original-source-independently-rechecked (:checking report)))
    (is (= :checked-conditional-abstract-derivation (:proof-status report)))
    (is (= transition-premises (:open-premises report)))
    (is (= transition-obligations (:open-obligations report)))
    (is (= #{:derivation-checked :declared-dependencies-checked
             :certificate-admission}
           (:closed-local-obligations report)))
    (is (= :not-performed (:assumption-discharge report)))
    (is (= :not-established (:implementation-correspondence report)))
    (is (= :not-established (:runtime-enforcement report)))
    (is (= :not-established (:meta-soundness report)))
    (is (= :in-process-checker-backed (:admission report)))
    (is (= :none (:authority report)))
    (is (not (contains? report :valid?)))
    (is (not (contains? report :transaction-authority)))
    (doseq [key [:fingerprint :subject-fingerprint :derivation-fingerprint
                 :source-fingerprint :dependency-graph-fingerprint]]
      (is (re-matches #"sha256:[0-9a-f]{64}" (get report key))
          (str key " " (pr-str (get report key)))))
    report))

(deftest genuine-guarded-transition-is-admitted-but-remains-conditional
  (let [pkg (package :guarded-transition transition-source)
        handle (cert/admit-certificate! pkg)
        report (checked-transition-report! handle)]
    (is (= (:fingerprint (subject/describe-subject request-subject))
           (:subject-fingerprint report)))
    (is (= (:fingerprint (derivation/check-transition-derivation!
                          transition-source))
           (:derivation-fingerprint report)))
    (is (= (:goal transition-source) (:goal report)))
    (is (identical? handle (cert/require-current-certificate!
                            handle transition-source (:current-graph pkg))))
    (is (= report (cert/describe-certificate handle)))))

(deftest genuine-propositional-proof-has-distinct-scope-and-obligations
  (let [pkg (package :propositional proposition-source)
        handle (cert/admit-certificate! pkg)
        report (cert/describe-certificate handle)]
    (is (= :propositional (:proof-kind report)))
    (is (= :typed-propositional-derivation-only (:scope report)))
    (is (= #{} (:open-premises report)))
    (is (= :checked-abstract-derivation (:proof-status report)))
    (is (= proposition-obligations (:open-obligations report)))
    (is (= :not-established (:meta-soundness report)))
    (is (= :none (:authority report)))
    (is (= handle (cert/require-current-certificate!
                   handle proposition-source (:current-graph pkg))))))

(deftest conditional-propositional-premises-remain-open
  (let [pkg (package :propositional proposition-with-premise)
        handle (cert/admit-certificate! pkg)
        report (cert/describe-certificate handle)]
    (is (= #{:premise/score} (:open-premises report)))
    (is (= :checked-conditional-abstract-derivation (:proof-status report)))
    (is (= proposition-obligations (:open-obligations report)))
    (is (= :none (:authority report)))
    (is (= handle (cert/require-current-certificate!
                   handle proposition-with-premise (:current-graph pkg))))))

(deftest omitted-and-fabricated-transition-premises-fail
  (let [pkg (package :guarded-transition transition-source)]
    (doseq [premises [#{} #{:premise/invented}
                      (conj transition-premises :premise/invented)]]
      (reject-certificate!
       #(cert/admit-certificate! (assoc pkg :expected-open-premises premises))
       :obligation-closure-mismatch))))

(deftest omitted-and-fabricated-external-obligations-fail
  (let [pkg (package :guarded-transition transition-source)]
    (doseq [obligation transition-obligations]
      (reject-certificate!
       #(cert/admit-certificate!
         (update pkg :expected-open-obligations disj obligation))
       :obligation-closure-mismatch))
    (doseq [obligations [(conj transition-obligations :certificate-admission)
                         (conj transition-obligations :verified)
                         (conj transition-obligations :xtdb-proved)
                         #{}]]
      (reject-certificate!
       #(cert/admit-certificate! (assoc pkg :expected-open-obligations obligations))
       :obligation-closure-mismatch))))

(deftest propositional-obligations-also-close-exactly
  (let [pkg (package :propositional proposition-source)]
    (doseq [obligation proposition-obligations]
      (reject-certificate!
       #(cert/admit-certificate!
         (update pkg :expected-open-obligations disj obligation))
       :obligation-closure-mismatch))
    (reject-certificate!
     #(cert/admit-certificate!
       (assoc pkg :expected-open-premises #{:premise/nonexistent}))
     :obligation-closure-mismatch)))

(deftest mismatched-premise-cannot-be-replaced-with-a-trusted-flag
  (let [pkg (package :propositional proposition-with-premise)]
    (reject-certificate!
     #(cert/admit-certificate! (assoc pkg :expected-open-premises #{}))
     :obligation-closure-mismatch)
    (reject-certificate!
     #(cert/admit-certificate! (assoc pkg :expected-open-obligations #{}))
     :obligation-closure-mismatch)))

(deftest copied-certificate-description-and-unissued-handles-do-not-authenticate
  (let [pkg (package :guarded-transition transition-source)
        handle (cert/admit-certificate! pkg)
        report (cert/describe-certificate handle)
        forged (gesso.assurance.proof.certificate.CertificateHandle.)]
    (is (not (cert/checked-certificate? report)))
    (is (not (cert/checked-certificate? forged)))
    (is (not (cert/checked-certificate? {:valid? true
                                         :type cert/certificate-type})))
    (reject-certificate! #(cert/describe-certificate report)
                         :unrecognized-certificate)
    (reject-certificate! #(cert/describe-certificate forged)
                         :unrecognized-certificate)
    (reject-certificate!
     #(cert/require-current-certificate! forged transition-source
                                          (:current-graph pkg))
     :unrecognized-certificate)
    (reject-certificate!
     #(cert/require-current-certificate! report transition-source
                                          (:current-graph pkg))
     :unrecognized-certificate)
    (is (cert/checked-certificate? handle))))

(deftest copied-graph-stamp-cannot-substitute-for-live-admission
  (let [pkg (package :guarded-transition transition-source)
        copied (dependency/describe-graph (:graph-handle pkg))]
    (is (not (dependency/checked-graph? copied)))
    (reject-certificate!
     #(cert/admit-certificate! (assoc pkg :graph-handle copied))
     :unadmitted-dependency-graph)
    (reject-certificate!
     #(cert/admit-certificate!
       (assoc pkg :graph-handle (gesso.assurance.dependency.CheckedGraph.)))
     :unadmitted-dependency-graph)
    (reject-certificate!
     #(cert/admit-certificate! (assoc pkg :graph-handle nil))
     :unadmitted-dependency-graph)))

(deftest input-shape-and-fabricated-verdict-flags-are-rejected
  (let [pkg (package :guarded-transition transition-source)]
    (doseq [key [:valid? :checked? :admission :authority :certificate
                 :assumption-discharge :runtime-enforcement
                 :implementation-correspondence :proof-status
                 :transaction-authority :fingerprint]]
      (reject-certificate! #(cert/admit-certificate! (assoc pkg key true))
                           :wrong-keys))
    (doseq [key [:proof-kind :proof-source :graph-handle :current-graph
                 :expected-open-premises :expected-open-obligations]]
      (reject-certificate! #(cert/admit-certificate! (dissoc pkg key))
                           :wrong-keys))
    (reject-certificate! #(cert/admit-certificate! (with-meta pkg {:valid? true}))
                         :metadata-not-allowed)
    (reject-certificate! #(cert/admit-certificate! (into (sorted-map) pkg))
                         :expected-plain-map)
    (reject-certificate! #(cert/admit-certificate! nil)
                         :expected-plain-map)))

(deftest obligated-sets-must-be-exact-plain-sets-without-metadata
  (let [pkg (package :guarded-transition transition-source)]
    (doseq [field [:expected-open-premises :expected-open-obligations]]
      (reject-certificate!
       #(cert/admit-certificate! (assoc pkg field (vec (get pkg field))))
       :obligation-closure-mismatch)
      (reject-certificate!
       #(cert/admit-certificate! (assoc pkg field
                                      (with-meta (get pkg field) {:checked? true})))
       :obligation-closure-mismatch))))

(deftest unsupported-proof-kind-is-not-a-new-proof-rule
  (let [pkg (package :guarded-transition transition-source)]
    (doseq [kind [:arbitrary-arithmetic :smt-valid :production-xtdb
                  :inductive-invariant :opaque-callback nil]]
      (reject-certificate! #(cert/admit-certificate! (assoc pkg :proof-kind kind))
                           :unsupported-proof-kind))))

(deftest source-checker-rejects-fabricated-transition-conclusions
  (let [pkg (package :guarded-transition transition-source)
        changed (assoc-in transition-source [:steps 1 :conclusion]
                          [:eq [:post :request/status] [:literal :open]])]
    (rejects-as!
     #(cert/admit-certificate! (assoc pkg :proof-source changed))
     :gesso.assurance.proof.derivation/invalid-derivation
     :wrong-assignment-conclusion)
    (rejects-as!
     #(cert/admit-certificate!
       (assoc pkg :proof-source
              (assoc-in transition-source [:steps 0 :rule] :arithmetic-axiom)))
     :gesso.assurance.proof.derivation/invalid-derivation
     :unknown-rule)))

(deftest source-checker-rejects-fabricated-propositional-inference
  (let [pkg (package :propositional proposition-source)
        changed (assoc-in proposition-source [:steps 0 :conclusion]
                          [:implies [:true] [:false]])]
    (rejects-as!
     #(cert/admit-certificate! (assoc pkg :proof-source changed))
     :gesso.assurance.proof.kernel/invalid-derivation
     :invalid-implication-reflexivity)))

(deftest unchecked-reports-cannot-be-submitted-as-source
  (let [pkg (package :guarded-transition transition-source)
        checked-report (derivation/check-transition-derivation! transition-source)
        other (package :propositional proposition-source)
        kernel-report (kernel/check-derivation! proposition-source)]
    (rejects-as!
     #(cert/admit-certificate! (assoc pkg :proof-source checked-report))
     :gesso.assurance.proof.derivation/invalid-derivation
     :wrong-keys)
    (rejects-as!
     #(cert/admit-certificate! (assoc other :proof-source kernel-report))
     :gesso.assurance.proof.kernel/invalid-derivation
     :wrong-keys)))

(deftest stale-proof-source-fails-after-valid-change-in-derivation
  (let [pkg (package :guarded-transition transition-source)
        handle (cert/admit-certificate! pkg)
        changed (-> transition-source
                    (assoc-in [:steps 0 :id] :step/new-guard)
                    (assoc-in [:steps 3 :from 0] :step/new-guard))]
    (is (= :abstract-transition-derivation-checked
           (:checking (derivation/check-transition-derivation! changed))))
    (reject-certificate!
     #(cert/require-current-certificate! handle changed (:current-graph pkg))
     :stale-certificate)
    (is (= handle
           (cert/require-current-certificate! handle transition-source
                                              (:current-graph pkg))))))

(deftest invalid-current-proof-fails-before-freshness-comparison
  (let [pkg (package :guarded-transition transition-source)
        handle (cert/admit-certificate! pkg)
        changed (assoc-in transition-source [:steps 0 :conclusion]
                          [:eq [:post :request/status] [:literal :open]])]
    (rejects-as!
     #(cert/require-current-certificate! handle changed (:current-graph pkg))
     :gesso.assurance.proof.derivation/invalid-derivation
     :wrong-guard-conclusion)))

(deftest mismatched-proof-subject-cannot-borrow-original-graph
  (let [pkg (package :guarded-transition transition-source)
        changed (assoc-in transition-source
                          [:transition :subject :interface :operation]
                          :example/different)]
    (is (= :abstract-transition-derivation-checked
           (:checking (derivation/check-transition-derivation! changed))))
    (reject-certificate!
     #(cert/admit-certificate! (assoc pkg :proof-source changed))
     :wrong-certificate-subject)))

(deftest different-root-or-extra-roots-cannot-be-certified
  (let [alt {:kind :rule :id :example/unrelated :interface {:value :other}}
        unrelated (raw-graph alt)
        pkg (package :guarded-transition transition-source unrelated)]
    (reject-certificate! #(cert/admit-certificate! pkg) :wrong-certificate-roots))
  (let [dependency-subject {:kind :rule :id :example/policy
                            :interface {:policy :claim-guard}}
        graph (raw-graph request-subject [dependency-subject])
        changed (assoc graph :roots #{:example/claim :example/policy})
        pkg (package :guarded-transition transition-source changed)]
    (reject-certificate! #(cert/admit-certificate! pkg)
                         :wrong-certificate-roots)))

(deftest stale-graph-dependency-rejects-freshness-even-with-identical-proof
  (let [dependency-subject {:kind :rule :id :example/policy
                            :interface {:claim-policy :only-open}}
        graph (raw-graph request-subject [dependency-subject])
        pkg (package :guarded-transition transition-source graph)
        handle (cert/admit-certificate! pkg)
        changed-subject (assoc-in dependency-subject [:interface :claim-policy]
                                  :allow-claimed)
        altered-graph (update graph :nodes
                              (fn [nodes]
                                (mapv (fn [node]
                                        (if (= :example/policy (:id node))
                                          (assoc node
                                                 :subject changed-subject
                                                 :subject-handle
                                                 (admission/admit-subject!
                                                  changed-subject))
                                          node)) nodes)))]
    (is (identical? handle
                    (cert/require-current-certificate! handle transition-source graph)))
    (rejects-as!
     #(cert/require-current-certificate! handle transition-source altered-graph)
     :gesso.assurance.dependency/invalid-dependency-graph
     :stale-dependency)))

(deftest source-revalidation-rejects-copied-identity-stamps
  (let [pkg (package :guarded-transition transition-source)
        fake (assoc-in transition-source
                       [:transition :subject :fingerprint]
                       (:subject-fingerprint (derivation/check-transition-derivation!
                                              transition-source)))]
    (rejects-as!
     #(cert/admit-certificate! (assoc pkg :proof-source fake))
     :gesso.assurance.subject/invalid-semantic-subject
     :wrong-keys)))

(deftest two-independently-issued-handles-are-distinct
  (let [pkg (package :guarded-transition transition-source)
        first-handle (cert/admit-certificate! pkg)
        next-handle (cert/admit-certificate! pkg)]
    (is (not (identical? first-handle next-handle)))
    (is (cert/checked-certificate? first-handle))
    (is (cert/checked-certificate? next-handle))
    (is (= (cert/describe-certificate first-handle)
           (cert/describe-certificate next-handle)))))

(deftest valid-graph-with-declared-dependencies-can-still-be-certified
  (let [policy {:kind :rule :id :example/policy
                :interface {:guard :only-open}}
        schema {:kind :schema :id :example/schema
                :interface {:owner :keyword}}
        graph (raw-graph request-subject [policy schema])
        pkg (package :guarded-transition transition-source graph)
        handle (cert/admit-certificate! pkg)]
    (checked-transition-report! handle)
    (is (= handle
           (cert/require-current-certificate! handle transition-source graph)))
    (is (= #{:example/claim}
           (:roots (dependency/describe-graph (:graph-handle pkg)))))))

(deftest no-report-can-claim-unconditional-authority
  (doseq [source [transition-source]]
    (let [pkg (package :guarded-transition source)
          report (cert/describe-certificate (cert/admit-certificate! pkg))]
      (is (= transition-premises (:open-premises report)))
      (is (= :not-established (:implementation-correspondence report)))
      (is (= :not-established (:meta-soundness report)))
      (is (= :none (:authority report))))))

(ns gesso.assurance.proof.induction-certificate-test
  "v1281: permanent adversarial tests for v1280 conditional-induction
   certificate admission. Verify source rechecking, dependency identities,
   stale evidence, exact unresolved premises, and in-process handle boundaries.

   A checked certificate is conditional evidence for one DECLARED abstract
   machine. These tests do not establish independent checker soundness, real
   initializer coverage, all physical writers, XTDB refinement, or authority."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.set :as set]
            [gesso.assurance.proof.induction :as induction]
            [gesso.assurance.proof.initial :as initial]
            [gesso.assurance.proof.unified-preservation :as preservation]
            [gesso.assurance.proof.operation-set :as operation-set]
            [gesso.assurance.proof.obligation :as obligation]
            [gesso.assurance.proof.certificate :as certificate]
            [gesso.assurance.admission :as admission]
            [gesso.assurance.dependency :as dependency]
            [gesso.assurance.subject :as subject]))

(def ^:private state-types
  {:request/status {:type :enum :name :request/status
                    :values #{:open :claimed :done}}
   :request/version {:type :integer}
   :request/owner {:type :keyword :nullable? true}})
(def ^:private invariant-version
  {:readers #{:request/version}
   :expression [:>= [:read :request/version] [:literal 0]]})
(def ^:private invariant-domain
  {:readers #{:request/status}
   :expression [:in [:read :request/status]
                [:literal #{:open :claimed :done}]]})
(def ^:private init
  {:readers #{:request/status :request/version}
   :expression [:and (:expression invariant-domain)
                (:expression invariant-version)]})
(def ^:private base-invariants
  {:request/domain invariant-domain
   :request/version-nonnegative invariant-version
   :request/truth {:readers #{} :expression [:literal true]}})
(defn- operation [id from to]
  {:subject {:kind :operation :id id :interface {:operation id}}
   :state-types state-types
   :input-types {}
   :guard {:readers #{:request/status}
           :expression [:= [:read :request/status] [:literal from]]}
   :reads #{:request/status}
   :writes #{:request/status}
   :replacements {:request/status [:literal to]}
   :frame #{:request/version :request/owner}})
(def ^:private claim (operation :request/claim :open :claimed))
(def ^:private unclaim (operation :request/unclaim :claimed :open))
(def ^:private catalog
  {:id :request/workflow
   :declared-operation-ids #{:request/claim :request/unclaim}
   :initial-condition init
   :invariants base-invariants
   :operations [{:id :request/claim :construction claim}
                {:id :request/unclaim :construction unclaim}]})

(def ^:private expected-extra
  #{:production-transition-correspondence
    :current-runtime-authorization
    :atomic-transaction-enforcement
    :checker-meta-soundness})

(defn- package [c]
  (let [root (certificate/induction-certificate-subject! c)
        graph {:roots #{(:id root)}
               :nodes [{:id (:id root) :subject root
                        :subject-handle (admission/admit-subject! root)
                        :depends-on []}]}
        report (induction/check-induction! c)]
    {:proof-kind :conditional-induction
     :proof-source c
     :graph-handle (dependency/check-graph! graph)
     :current-graph graph
     :expected-open-premises
     #{:gesso.assurance.proof.induction/matching-declared-abstract-machine}
     :expected-open-obligations
     (set/union expected-extra
                (disj (:open-meta-obligations report)
                      :certificate-admission))}))

(defn- error [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn- reject! [f type reason]
  (let [data (error f)]
    (is (map? data) (str "Expected ExceptionInfo: " reason))
    (is (= type (:error/type data)) (pr-str data))
    (is (= reason (:reason data)) (pr-str data))
    data))

(defn- reject-cert! [f reason]
  (reject! f :gesso.assurance.proof.certificate/invalid-certificate reason))

(defn- graph-for
  ([root] (graph-for root []))
  ([root dependencies]
   (let [edges (mapv (fn [s]
                       {:id (:id s)
                        :fingerprint (:fingerprint (subject/describe-subject s))})
                     dependencies)]
     {:roots #{(:id root)}
      :nodes (into [{:id (:id root) :subject root
                     :subject-handle (admission/admit-subject! root)
                     :depends-on edges}]
                   (mapv (fn [s] {:id (:id s) :subject s
                                  :subject-handle (admission/admit-subject! s)
                                  :depends-on []})
                         dependencies))})))

(defn- package-with-graph [c graph]
  (assoc (package c)
         :graph-handle (dependency/check-graph! graph)
         :current-graph graph))

(def ^:private premise
  :gesso.assurance.proof.induction/matching-declared-abstract-machine)

(def ^:private external-conditions
  ;; The expected set includes *every* original source-owned premise. Do not
  ;; silently replace the inherited obligations with an abbreviated list.
  (set/union expected-extra
             (disj (:open-meta-obligations (induction/check-induction! catalog))
                   :certificate-admission)))

(deftest complete-source-can-be-admitted-with-only-conditional-scope
  (let [p (package catalog)
        cert (certificate/admit-certificate! p)
        report (certificate/describe-certificate cert)]
    (is (certificate/checked-certificate? cert))
    (is (= :conditional-induction (:proof-kind report)))
    (is (= :closed-declared-abstract-machine-only (:scope report)))
    (is (= :checked-conditional-abstract-induction (:proof-status report)))
    (is (= (set (keys base-invariants)) (get-in report [:goal :invariant-ids])))
    (is (= (:expected-open-obligations p) (:open-obligations report)))
    (is (= (:expected-open-premises p) (:open-premises report)))
    (is (contains? (:open-obligations report) :checker-meta-soundness))
    (is (contains? (:open-obligations report) :physical-write-path-coverage))
    (is (contains? (:open-obligations report) :implementation-correspondence))
    (is (not (contains? (:open-obligations report) :certificate-admission)))
    (is (= #{:induction-rechecked :declared-dependencies-checked
             :certificate-admission} (:closed-local-obligations report)))
    (is (= :not-established (:implementation-correspondence report)))
    (is (= :none (:authority report)))
    (is (= (:fingerprint (subject/describe-subject
                         (certificate/induction-certificate-subject! catalog)))
           (:subject-fingerprint report)))
    (is (identical? cert (certificate/require-current-certificate!
                          cert catalog (:current-graph p))))))

(deftest incomplete-base-not-promoted-into-certificate
  (let [c (assoc catalog :invariants
                 (assoc base-invariants :request/owner-present
                        {:readers #{:request/owner}
                         :expression [:not= [:read :request/owner] [:literal nil]]}))]
    (is (= :unsafe-induction-judgment
           (:reason (error #(certificate/induction-certificate-subject! c)))))
    (is (= :unsafe-induction-judgment
           (:reason (error #(certificate/admit-certificate!
                              (assoc (package catalog) :proof-source c))))))))

(deftest incomplete-preservation-not-promoted-into-certificate
  (let [bad (-> (operation :request/unclaim :claimed :done)
                (assoc :input-types {:input/new-version {:type :integer}})
                (assoc :reads #{:request/status :input/new-version})
                (assoc :writes #{:request/status :request/version})
                (assoc :replacements {:request/status [:literal :done]
                                      :request/version [:read :input/new-version]})
                (assoc :frame #{:request/owner}))
        c (assoc catalog :operations
                 [{:id :request/claim :construction claim}
                  {:id :request/unclaim :construction bad}])]
    (is (= :unsafe-induction-judgment
           (:reason (error #(certificate/induction-certificate-subject! c)))))))

(deftest copied-induction-report-cannot-be-proof-source
  (is (= :wrong-keys
         (:reason (error #(certificate/admit-certificate!
                            (assoc (package catalog) :proof-source
                                   (induction/check-induction! catalog))))))))

(deftest substitution-of-public-induction-checker-rejected
  (let [p (package catalog)
        original induction/check-induction!]
    (with-redefs [induction/check-induction!
                  (fn [c] (assoc (original c) :authority :all))]
      ;; Original is frozen; fake response must never become accepted.
      (is (= :induction-checker-contract-changed
             (:reason (error #(certificate/admit-certificate! p))))))))

(deftest open-obligation-declaration-must-match
  (let [p (package catalog)]
    (is (= :obligation-closure-mismatch
           (:reason (error #(certificate/admit-certificate!
                              (update p :expected-open-obligations
                                      disj :checker-meta-soundness))))))
    (is (= :obligation-closure-mismatch
           (:reason (error #(certificate/admit-certificate!
                              (assoc p :expected-open-premises #{}))))))))

(deftest altered-source-cannot-freshen-existing-certificate
  (let [p (package catalog)
        cert (certificate/admit-certificate! p)
        changed (assoc catalog :operations
                       [{:id :request/claim :construction
                         (assoc-in claim [:subject :interface :operation] :request/claim-v2)}
                        {:id :request/unclaim :construction unclaim}])]
    (is (= :wrong-certificate-subject
           (:reason (error #(certificate/require-current-certificate!
                              cert changed (:current-graph p))))))
    (is (identical? cert
                    (certificate/require-current-certificate!
                      cert catalog (:current-graph p))))))

(deftest wrong-root-rejected
  (let [p (package catalog)
        other (certificate/induction-certificate-subject!
                (assoc catalog :id :request/other))
        graph {:roots #{(:id other)}
               :nodes [{:id (:id other) :subject other
                        :subject-handle (admission/admit-subject! other)
                        :depends-on []}]}]
    (is (= :wrong-certificate-roots
           (:reason (error #(certificate/admit-certificate!
                              (-> p
                                  (assoc :graph-handle (dependency/check-graph! graph))
                                  (assoc :current-graph graph)))))))))

(deftest forged-proof-status-fields-cannot-be-certificate-source
  (let [p (package catalog)]
    (is (= :wrong-keys
           (:reason (error #(certificate/admit-certificate!
                              (assoc p :certificate :not-issued))))))
    (is (= :wrong-keys
           (:reason (error #(certificate/admit-certificate!
                              (assoc p :proof-source
                                     (assoc catalog :proof-status :proved)))))))))

(deftest complete-conditional-certificate-has-exact-open-boundaries
  (let [p (package catalog)
        h (certificate/admit-certificate! p)
        d (certificate/describe-certificate h)
        source-report (induction/check-induction! catalog)]
    (is (= external-conditions (:open-obligations d)))
    (is (= #{premise} (:open-premises d)))
    (is (= #{:induction-rechecked :declared-dependencies-checked
             :certificate-admission}
           (:closed-local-obligations d)))
    (is (= :all-declared-invariants-inductive (get-in d [:goal :kind])))
    (is (= (:declared-invariant-ids source-report)
           (get-in d [:goal :invariant-ids])))
    (is (= :request/workflow (get-in d [:goal :catalog-id])))
    (is (= (:fingerprint source-report) (:derivation-fingerprint d)))
    (is (= :source-relative-abstract-proof-only (:certificate-scope d)))
    (is (= :original-source-independently-rechecked (:checking d)))
    (is (= :not-performed (:assumption-discharge d)))
    (is (= :not-established (:runtime-enforcement d)))
    (is (= :not-established (:meta-soundness d)))
    (is (= :in-process-checker-backed (:admission d)))
    (is (= :none (:authority d)))
    (is (every? #(re-matches #"sha256:[0-9a-f]{64}" (get d %))
                [:fingerprint :subject-fingerprint :derivation-fingerprint
                 :source-fingerprint :dependency-graph-fingerprint]))
    (is (not (contains? d :verified?)))
    (is (not (contains? d :transaction-authority)))
    (is (identical? h (certificate/require-current-certificate!
                       h catalog (:current-graph p))))))

(deftest a-report-and-an-unregistered-handle-cannot-be-used-as-certificates
  (let [p (package catalog)
        issued (certificate/admit-certificate! p)
        report (certificate/describe-certificate issued)
        fake (gesso.assurance.proof.certificate.CertificateHandle.)]
    (doseq [candidate [nil {} report (induction/check-induction! catalog) fake]]
      (is (false? (certificate/checked-certificate? candidate)))
      (reject-cert! #(certificate/describe-certificate candidate)
                    :unrecognized-certificate)
      (reject-cert! #(certificate/require-current-certificate!
                     candidate catalog (:current-graph p))
                    :unrecognized-certificate))
    (is (certificate/checked-certificate? issued))
    (is (identical? issued (certificate/require-current-certificate!
                            issued catalog (:current-graph p))))))

(deftest copied-report-with-forged-success-and-authority-is-not-source
  (let [p (package catalog)
        source-report (induction/check-induction! catalog)
        report (-> source-report
                   (assoc :proof-status :proved
                          :certificate :issued
                          :admission :admitted
                          :authority :production))]
    (reject! #(certificate/admit-certificate!
               (assoc p :proof-source report))
             :gesso.assurance.proof.operation-set/invalid-operation-set
             :wrong-keys)
    (reject! #(certificate/admit-certificate!
               (assoc p :proof-source
                      (assoc catalog :authority :production)))
             :gesso.assurance.proof.operation-set/invalid-operation-set
             :wrong-keys)
    (reject-cert! #(certificate/admit-certificate!
                   (assoc p :admission :admitted))
                  :wrong-keys)))

(deftest copied-dependency-description-is-not-a-live-handle
  (let [p (package catalog)
        description (dependency/describe-graph (:graph-handle p))]
    (is (not (dependency/checked-graph? description)))
    (doseq [bogus [nil {} description
                   (gesso.assurance.dependency.CheckedGraph.)]]
      (reject-cert! #(certificate/admit-certificate!
                     (assoc p :graph-handle bogus))
                    :unadmitted-dependency-graph))))

(deftest certificate-package-rejects-extra-missing-and-metadata-bearing-keys
  (let [p (package catalog)]
    (doseq [key [:valid? :certificate :proof-status :authority
                 :transaction-authority :derivation-fingerprint]]
      (reject-cert! #(certificate/admit-certificate! (assoc p key true))
                    :wrong-keys))
    (doseq [key [:proof-kind :proof-source :graph-handle :current-graph
                 :expected-open-premises :expected-open-obligations]]
      (reject-cert! #(certificate/admit-certificate! (dissoc p key))
                    :wrong-keys))
    (reject-cert! #(certificate/admit-certificate! (with-meta p {:certified true}))
                  :metadata-not-allowed)))

(deftest obligation-sets-are-closed-without-implicit-discharge
  (let [p (package catalog)]
    (doseq [missing external-conditions]
      (reject-cert! #(certificate/admit-certificate!
                     (update p :expected-open-obligations disj missing))
                    :obligation-closure-mismatch))
    (doseq [value [(conj external-conditions :some-other-proof)
                   #{:checker-meta-soundness}
                   #{}
                   nil
                   (vec external-conditions)
                   (with-meta external-conditions {:checked true})]]
      (reject-cert! #(certificate/admit-certificate!
                     (assoc p :expected-open-obligations value))
                    :obligation-closure-mismatch))
    (doseq [value [#{} #{:fake/premise}
                   (conj #{premise} :fake/premise)
                   [premise]
                   nil
                   (with-meta #{premise} {:checked true})]]
      (reject-cert! #(certificate/admit-certificate!
                     (assoc p :expected-open-premises value))
                    :obligation-closure-mismatch))
    (is (= external-conditions
           (:open-obligations (certificate/describe-certificate
                              (certificate/admit-certificate! p)))))))

(deftest obsolete-or-unsupported-proof-kind-never-gets-induction-admission
  (let [p (package catalog)]
    (doseq [k [:induction :proved-induction :application-invariant
               :universal-refinement nil]]
      (reject-cert! #(certificate/admit-certificate!
                     (assoc p :proof-kind k))
                    :unsupported-proof-kind))))

(deftest partial-induction-reports-cannot-be-upgraded-using-honest-package
  (let [bad-invariant
        (assoc catalog :invariants
               (assoc base-invariants
                      :request/owner-present
                      {:readers #{:request/owner}
                       :expression [:not= [:read :request/owner]
                                    [:literal nil]]}))
        invalid-unclaim
        (-> (operation :request/unclaim :claimed :done)
            (assoc :input-types {:input/new-version {:type :integer}}
                   :reads #{:request/status :input/new-version}
                   :writes #{:request/status :request/version}
                   :replacements {:request/status [:literal :done]
                                  :request/version [:read :input/new-version]}
                   :frame #{:request/owner}))
        bad-operation
        (assoc catalog :operations
               [{:id :request/claim :construction claim}
                {:id :request/unclaim :construction invalid-unclaim}])
        p (package catalog)]
    (doseq [[reason c] [[:base bad-invariant] [:step bad-operation]]]
      (testing (name reason)
        (let [r (induction/check-induction! c)]
          (is (not (:all-declared-invariants-conditionally-inductive? r)))
          (is (seq (:unsupported-inductive-invariant-ids r))))
        (reject-cert! #(certificate/induction-certificate-subject! c)
                      :unsafe-induction-judgment)
        (reject-cert! #(certificate/admit-certificate!
                       (assoc p :proof-source c))
                      :unsafe-induction-judgment)))))

(deftest exact-operation-coverage-missing-extra-or-duplicated-is-rejected
  (let [p (package catalog)
        removed (assoc catalog :operations [(first (:operations catalog))])
        added (assoc catalog :operations
                     (conj (:operations catalog)
                           {:id :request/extra
                            :construction (operation :request/extra :done :open)}))
        duplicate (update catalog :operations conj (first (:operations catalog)))]
    (doseq [[c reason] [[removed :operation-id-coverage-mismatch]
                        [added :operation-id-coverage-mismatch]
                        [duplicate :duplicate-operation-id]]]
      (reject! #(certificate/admit-certificate! (assoc p :proof-source c))
               :gesso.assurance.proof.operation-set/invalid-operation-set
               reason))))

(deftest source-change-with-same-catalog-id-invalidates-issued-handle
  (let [p (package catalog)
        issued (certificate/admit-certificate! p)
        changes [(assoc-in catalog [:operations 0 :construction :subject
                                    :interface :operation] :request/claim-v2)
                 (assoc catalog :invariants
                        (assoc base-invariants :request/extra-truth
                               {:readers #{} :expression [:literal true]}))]]
    (doseq [changed changes]
      (reject-cert! #(certificate/require-current-certificate!
                     issued changed (:current-graph p))
                    :wrong-certificate-subject)
      (is (identical? issued
                      (certificate/require-current-certificate!
                       issued catalog (:current-graph p)))))))

(deftest adding-or-removing-declared-machine-operation-does-not-borrow-old-root
  (let [p (package catalog)
        issued (certificate/admit-certificate! p)
        third {:id :request/done
               :construction (operation :request/done :claimed :done)}
        changed (-> catalog
                    (assoc :declared-operation-ids
                           #{:request/claim :request/unclaim :request/done})
                    (update :operations conj third))]
    (is (true? (:all-declared-invariants-conditionally-inductive?
                (induction/check-induction! changed))))
    (reject-cert! #(certificate/require-current-certificate!
                   issued changed (:current-graph p))
                  :wrong-certificate-subject)))

(deftest dependency-freshness-requires-original-live-graph
  (let [root (certificate/induction-certificate-subject! catalog)
        policy {:kind :rule :id :policy/claim
                :interface {:policy :one-owner}}
        graph (graph-for root [policy])
        p (package-with-graph catalog graph)
        issued (certificate/admit-certificate! p)
        changed-policy (assoc-in policy [:interface :policy] :unrestricted)
        graph-changed (update graph :nodes
                              (fn [nodes]
                                (mapv (fn [node]
                                        (if (= :policy/claim (:id node))
                                          (assoc node
                                                 :subject changed-policy
                                                 :subject-handle
                                                 (admission/admit-subject! changed-policy))
                                          node))
                                      nodes)))]
    (is (= (:fingerprint (subject/describe-subject policy))
           (get-in graph [:nodes 0 :depends-on 0 :fingerprint])))
    (is (identical? issued
                    (certificate/require-current-certificate!
                     issued catalog graph)))
    (reject! #(certificate/require-current-certificate!
              issued catalog graph-changed)
             :gesso.assurance.dependency/invalid-dependency-graph
             :stale-dependency)))

(deftest correctly-declared-extra-dependencies-remain-conditional
  (let [root (certificate/induction-certificate-subject! catalog)
        policy {:kind :rule :id :policy/claim
                :interface {:policy :one-owner}}
        schema {:kind :schema :id :schema/request
                :interface {:fields #{:request/status :request/version}}}
        graph (graph-for root [policy schema])
        p (package-with-graph catalog graph)
        issued (certificate/admit-certificate! p)
        d (certificate/describe-certificate issued)]
    (is (= :conditional-induction (:proof-kind d)))
    (is (= external-conditions (:open-obligations d)))
    (is (= #{premise} (:open-premises d)))
    (is (identical? issued (certificate/require-current-certificate!
                            issued catalog graph)))
    (is (not (:implemented? d)))
    (is (= :none (:authority d)))))

(deftest wrong-root-or-two-roots-cannot-be-admitted
  (let [p (package catalog)
        root (certificate/induction-certificate-subject! catalog)
        other (certificate/induction-certificate-subject!
               (assoc catalog :id :request/different-workflow))
        extra {:kind :rule :id :policy/misc :interface {:value true}}
        unrelated (graph-for other)
        double (assoc (graph-for root [extra]) :roots
                      #{(:id root) :policy/misc})]
    (doseq [[graph reason] [[unrelated :wrong-certificate-roots]
                            [double :wrong-certificate-roots]]]
      (let [p2 (package-with-graph catalog graph)]
        (reject-cert! #(certificate/admit-certificate! p2) reason)))))

(deftest reordered-operations-conservatively-stale-source-bound-certificate
  (let [p (package catalog)
        issued (certificate/admit-certificate! p)
        reordered (assoc catalog :operations (vec (reverse (:operations catalog))))]
    (is (= (induction/check-induction! catalog)
           (induction/check-induction! reordered)))
    ;; The induction result is order-independent, but the certificate binds
    ;; the original SOURCE ordering. Stale rejection is conservative.
    (reject-cert! #(certificate/require-current-certificate!
                   issued reordered (:current-graph p))
                  :stale-certificate)
    (is (identical? issued (certificate/require-current-certificate!
                            issued catalog (:current-graph p))))))

(deftest public-induction-checker-swap-cannot-forge-admission-or-freshness
  (let [p (package catalog)
        h (certificate/admit-certificate! p)
        original induction/check-induction!]
    (with-redefs [induction/check-induction!
                  (fn [c] (assoc (original c) :authority :infinite))]
      (reject-cert! #(certificate/admit-certificate! p)
                    :induction-checker-contract-changed)
      (reject-cert! #(certificate/require-current-certificate!
                     h catalog (:current-graph p))
                    :induction-checker-contract-changed))
    (is (identical? h (certificate/require-current-certificate!
                       h catalog (:current-graph p))))))

(deftest checker-outcome-that-still-looks-successful-is-not-independent-proof
  (let [p (package catalog)
        h (certificate/admit-certificate! p)
        original induction/check-induction!]
    (with-redefs [induction/check-induction!
                  (fn [c] (-> (original c)
                              (assoc :checker-soundness :formally-proved)
                              (assoc :implementation-correspondence :established)))]
      (reject-cert! #(certificate/require-current-certificate!
                     h catalog (:current-graph p))
                    :induction-checker-contract-changed))
    (is (= :not-established
           (:implementation-correspondence
            (certificate/describe-certificate h))))))

(deftest issued-certificate-never-launders-external-assumptions
  (let [p (package catalog)
        h (certificate/admit-certificate! p)
        d (certificate/describe-certificate h)]
    (is (= (:expected-open-premises p) (:open-premises d)))
    (is (= (:expected-open-obligations p) (:open-obligations d)))
    (is (= :not-performed (:assumption-discharge d)))
    (is (= :not-established (:implementation-correspondence d)))
    (is (= :not-established (:runtime-enforcement d)))
    (is (= :not-established (:meta-soundness d)))
    (is (= :none (:authority d)))
    (is (not (contains? d :valid?)))
    (is (not (contains? d :verified?)))
    (is (not (contains? d :transaction-authority)))
    (is (not (contains? d :write-permission)))))

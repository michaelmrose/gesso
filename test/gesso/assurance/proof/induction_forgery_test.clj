(ns gesso.assurance.proof.induction-forgery-test
  "v1312 permanent base/catalog/authority forgery corpus.

   All positive admissions are ORIGINAL source-bound conditional induction
   certificates. Mutations challenge the exact initialization, catalog,
   admitted dependency graph, in-process handle and checker-freshness boundary.
   No report or test assertion supplies independent proof of rule soundness,
   universal runtime coverage, physical XTDB correspondence or write authority."
  (:require [clojure.set :as set]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [gesso.assurance.admission :as admission]
            [gesso.assurance.dependency :as dependency]
            [gesso.assurance.proof.certificate :as certificate]
            [gesso.assurance.proof.induction :as induction]
            [gesso.assurance.proof.initial :as initial]
            [gesso.assurance.proof.operation-set :as operation-set]
            [gesso.assurance.proof.unified-preservation :as preservation]))

(def ^:private state-types
  {:request/status {:type :enum :name :request/status
                    :values #{:open :claimed :done}}
   :request/version {:type :integer}
   :request/owner {:type :keyword :nullable? true}})
(def ^:private status-domain
  {:readers #{:request/status}
   :expression [:in [:read :request/status]
                [:literal #{:open :claimed :done}]]})
(def ^:private nonnegative-version
  {:readers #{:request/version}
   :expression [:>= [:read :request/version] [:literal 0]]})
(def ^:private base
  {:readers #{:request/status :request/version}
   :expression [:and (:expression status-domain)
                (:expression nonnegative-version)]})
(def ^:private invariants
  {:request/status-domain status-domain
   :request/version-nonnegative nonnegative-version
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

(def ^:private catalog
  {:id :request/induction-forgery
   :declared-operation-ids #{:request/claim :request/unclaim}
   :initial-condition base
   :invariants invariants
   :operations [{:id :request/claim
                 :construction (operation :request/claim :open :claimed)}
                {:id :request/unclaim
                 :construction (operation :request/unclaim :claimed :open)}]})

(def ^:private external-obligations
  #{:production-transition-correspondence
    :current-runtime-authorization
    :atomic-transaction-enforcement
    :checker-meta-soundness})
(def ^:private premise
  :gesso.assurance.proof.induction/matching-declared-abstract-machine)

(defn- package [source]
  (let [root (certificate/induction-certificate-subject! source)
        graph {:roots #{(:id root)}
               :nodes [{:id (:id root)
                        :subject root
                        :subject-handle (admission/admit-subject! root)
                        :depends-on []}]}
        induction-report (induction/check-induction! source)]
    {:proof-kind :conditional-induction
     :proof-source source
     :graph-handle (dependency/check-graph! graph)
     :current-graph graph
     :expected-open-premises #{premise}
     :expected-open-obligations
     (set/union external-obligations
                (disj (:open-meta-obligations induction-report)
                      :certificate-admission))}))

(def ^:private valid-package (delay (package catalog)))

;; Force the one-time source fixture before with-redefs tests execute; a Delay
;; first realized inside a forged public checker would cache that exception.
(use-fixtures :once (fn [run] @valid-package (run)))
(defn- checked! [c] (induction/check-induction! c))
(defn- error-data [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))
(defn- rejects! [f type reason]
  (let [data (error-data f)]
    (is (map? data) (str "Expected typed rejection: " reason))
    (is (= type (:error/type data)) (pr-str data))
    (is (= reason (:reason data)) (pr-str data))
    data))
(defn- cert-rejects! [f reason]
  (rejects! f :gesso.assurance.proof.certificate/invalid-certificate reason))
(defn- row [r id]
  (first (filter #(= id (:invariant %)) (:induction-outcomes r))))
(defn- no-authority! [r]
  (is (= :none (:authority r)))
  (is (= :not-established (:implementation-correspondence r)))
  (is (= :not-established (:runtime-enforcement r)))
  (is (not (contains? r :transaction-authority))))
(defn- unsafe-base []
  (assoc catalog :initial-condition
         {:readers #{} :expression [:literal true]}))
(defn- unsupported-step []
  (-> catalog
      (assoc-in [:operations 1 :construction :input-types]
                {:input/new-version {:type :integer}})
      (assoc-in [:operations 1 :construction :reads]
                #{:request/status :input/new-version})
      (assoc-in [:operations 1 :construction :writes]
                #{:request/status :request/version})
      (assoc-in [:operations 1 :construction :replacements]
                {:request/status [:literal :open]
                 :request/version [:read :input/new-version]})
      (assoc-in [:operations 1 :construction :frame]
                #{:request/owner})))
(defn- valid-other-guard []
  ;; The altered guard remains a valid restricted transition, but its ORIGINAL
  ;; source identity is not the identity under which the handle was issued.
  (assoc-in catalog [:operations 0 :construction :guard :expression]
            [:= [:read :request/status] [:literal :done]]))

(deftest original-source-produces-only-conditional-abstract-evidence
  (let [r (checked! catalog)
        p @valid-package
        h (certificate/admit-certificate! p)
        d (certificate/describe-certificate h)]
    (is (= induction/report-type (:type r)))
    (is (:all-declared-invariants-conditionally-inductive? r))
    (is (= (:declared-operation-ids catalog) (:declared-operation-ids r)))
    (is (= (set (keys invariants))
           (:conditionally-checked-inductive-invariant-ids r)))
    (is (= :conditional-induction (:proof-kind d)))
    (is (certificate/checked-certificate? h))
    (is (identical? h (certificate/require-current-certificate!
                       h catalog (:current-graph p))))
    (is (= #{premise} (:open-premises d)))
    (is (set/subset? external-obligations (:open-obligations d)))
    (is (= :not-established (:meta-soundness d)))
    (no-authority! r)
    (no-authority! d)))

(deftest strengthened-base-is-a-new-source-even-if-result-remains-inductive
  (let [changed (assoc-in catalog [:initial-condition :expression]
                          [:and (:expression base) [:literal true]])
        original (checked! catalog)
        current (checked! changed)
        p @valid-package
        h (certificate/admit-certificate! p)]
    (is (:all-declared-invariants-conditionally-inductive? current))
    (is (not= (:fingerprint original) (:fingerprint current)))
    (cert-rejects! #(certificate/require-current-certificate!
                     h changed (:current-graph p))
                   :wrong-certificate-subject)
    (is (identical? h (certificate/require-current-certificate!
                       h catalog (:current-graph p))))))

(deftest weakening-base-keeps-original-base-obligation-open
  (let [r (checked! (unsafe-base))]
    (is (false? (:all-declared-invariants-conditionally-inductive? r)))
    (is (contains? (:unsupported-inductive-invariant-ids r)
                   :request/version-nonnegative))
    (is (= :initial-consequence-open
           (:reason (row r :request/version-nonnegative))))
    (is (= :open (:status (row r :request/version-nonnegative))))
    (cert-rejects! #(certificate/induction-certificate-subject! (unsafe-base))
                   :unsafe-induction-judgment)))

(deftest an-impossible-base-does-not-become-a-vacuous-proof
  (let [changed (assoc catalog :initial-condition
                       {:readers #{} :expression [:literal false]})
        r (checked! changed)]
    (is (false? (:all-declared-invariants-conditionally-inductive? r)))
    (is (seq (:unsupported-inductive-invariant-ids r)))
    (cert-rejects! #(certificate/admit-certificate!
                     (assoc @valid-package :proof-source changed))
                   :unsafe-induction-judgment)))

(deftest an-extra-base-claim-cannot-borrow-existing-induction
  (let [changed (assoc-in catalog [:invariants :request/always-claimed]
                          {:readers #{:request/status}
                           :expression [:= [:read :request/status]
                                        [:literal :claimed]]})
        r (checked! changed)]
    (is (= :initial-consequence-open
           (:reason (row r :request/always-claimed))))
    (is (contains? (:unsupported-inductive-invariant-ids r)
                   :request/always-claimed))
    (cert-rejects! #(certificate/admit-certificate!
                     (assoc @valid-package :proof-source changed))
                   :unsafe-induction-judgment)))

(deftest an-unsafe-operation-keeps-one-invariant-open
  (let [r (checked! (unsupported-step))
        version (row r :request/version-nonnegative)]
    (is (= :some-preservation-open (:reason version)))
    (is (= #{:request/unclaim} (:unsupported-operation-ids version)))
    (is (= :open (:status version)))
    (cert-rejects! #(certificate/induction-certificate-subject!
                     (unsupported-step)) :unsafe-induction-judgment)))

(deftest forged-base-and-step-success-report-cannot-be-a-catalog
  (let [good (checked! catalog)
        bad (checked! (unsupported-step))
        forged (-> bad
                   (assoc :fingerprint (:fingerprint good)
                          :all-declared-invariants-conditionally-inductive? true
                          :unsupported-inductive-invariant-ids #{}
                          :conditionally-checked-inductive-invariant-ids
                          (set (keys invariants))
                          :proof-status :proved
                          :certificate :issued
                          :authority :production))]
    (is (= :proved (:proof-status forged)))
    (no-authority! good)
    (rejects! #(certificate/admit-certificate!
                (assoc @valid-package :proof-source forged))
              :gesso.assurance.proof.operation-set/invalid-operation-set
              :wrong-keys)))

(deftest source-embedded-authority-fields-do-not-expand-the-language
  (doseq [[label source]
          [[:root (assoc catalog :authority :production)]
           [:proof (assoc catalog :proof-status :verified)]
           [:operation (assoc-in catalog [:operations 0 :construction :authority]
                                 :production)]
           [:initializer (assoc-in catalog [:initial-condition :certified?] true)]
           [:invariant (assoc-in catalog [:invariants :request/truth :verified?]
                               true)]]]
    (testing (name label)
      (let [data (error-data #(checked! source))]
        (is (map? data) (str label))
        (is (qualified-keyword? (:error/type data)) (pr-str data))))))

(deftest missing-or-duplicated-operation-is-not-a-complete-catalog
  (let [cases [(assoc catalog :operations [(first (:operations catalog))])
               (update catalog :operations conj (first (:operations catalog)))
               (update catalog :declared-operation-ids disj :request/claim)
               (update catalog :operations conj
                       {:id :request/extra
                        :construction (operation :request/extra :done :open)})]]
    (doseq [source cases]
      (let [data (error-data #(checked! source))]
        (is (= :gesso.assurance.proof.operation-set/invalid-operation-set
               (:error/type data)) (pr-str data))))))

(deftest independently-valid-operation-guard-invalidates-prior-certificate
  (let [changed (valid-other-guard)
        p @valid-package
        h (certificate/admit-certificate! p)]
    (is (:all-declared-invariants-conditionally-inductive? (checked! changed)))
    (is (not= (:fingerprint (checked! catalog))
              (:fingerprint (checked! changed))))
    (cert-rejects! #(certificate/require-current-certificate!
                     h changed (:current-graph p))
                   :wrong-certificate-subject)))

(deftest catalog-id-only-match-is-not-a-freshness-proof
  (let [p @valid-package
        h (certificate/admit-certificate! p)
        changed (assoc-in catalog [:operations 0 :construction :subject
                                   :interface :operation] :request/other-claim)]
    (is (= (:id catalog) (:id changed)))
    (is (not= catalog changed))
    (is (map? (error-data #(certificate/require-current-certificate!
                           h changed (:current-graph p)))))))

(deftest metadata-and-extra-package-flags-cannot-issue-a-certificate
  (let [p @valid-package]
    (cert-rejects! #(certificate/admit-certificate!
                     (with-meta p {:proven true}))
                   :metadata-not-allowed)
    (doseq [k [:valid? :proved :authority :physical-correspondence
               :certificate :transaction-authority]]
      (cert-rejects! #(certificate/admit-certificate! (assoc p k true))
                     :wrong-keys))
    (doseq [k (keys p)]
      (cert-rejects! #(certificate/admit-certificate! (dissoc p k))
                     :wrong-keys))))

(deftest forged-open-obligation-closures-are-rejected
  (let [p @valid-package
        original (:expected-open-obligations p)]
    (is (contains? original :checker-meta-soundness))
    (is (contains? original :physical-write-path-coverage))
    (doseq [obligation original]
      (cert-rejects!
       #(certificate/admit-certificate!
         (update p :expected-open-obligations disj obligation))
       :obligation-closure-mismatch))
    (doseq [invalid [#{} #{:forgery/proved}
                     (conj original :forgery/proved)
                     (with-meta original {:discharged true})
                     (vec original)]]
      (cert-rejects!
       #(certificate/admit-certificate!
         (assoc p :expected-open-obligations invalid))
       :obligation-closure-mismatch))
    (doseq [invalid [#{} #{:forgery/premise}
                     (conj #{premise} :forgery/premise)
                     (with-meta #{premise} {:closed true})]]
      (cert-rejects!
       #(certificate/admit-certificate!
         (assoc p :expected-open-premises invalid))
       :obligation-closure-mismatch))))

(deftest bogus-handles-and-descriptions-never-authenticate
  (let [p @valid-package
        real (certificate/admit-certificate! p)
        description (certificate/describe-certificate real)
        graph-description (dependency/describe-graph (:graph-handle p))
        bogus (gesso.assurance.proof.certificate.CertificateHandle.)]
    (is (certificate/checked-certificate? real))
    (doseq [candidate [nil {} description (checked! catalog) bogus
                       (assoc description :authority :production
                              :meta-soundness :proved)]]
      (is (false? (certificate/checked-certificate? candidate)))
      (cert-rejects! #(certificate/describe-certificate candidate)
                     :unrecognized-certificate))
    (is (not (dependency/checked-graph? graph-description)))
    (doseq [graph [nil {} graph-description
                   (gesso.assurance.dependency.CheckedGraph.)]]
      (cert-rejects! #(certificate/admit-certificate!
                       (assoc p :graph-handle graph))
                     :unadmitted-dependency-graph))))

(deftest changing-the-live-graph-does-not-preserve-certificate-freshness
  (let [p @valid-package
        h (certificate/admit-certificate! p)
        root (certificate/induction-certificate-subject! catalog)
        other-root (assoc root :id :forgery/replacement-root)
        other-graph {:roots #{(:id other-root)}
                     :nodes [{:id (:id other-root)
                              :subject other-root
                              :subject-handle (admission/admit-subject! other-root)
                              :depends-on []}]}]
    (rejects! #(certificate/require-current-certificate!
                 h catalog other-graph)
              :gesso.assurance.dependency/invalid-dependency-graph
              :stale-graph)
    (is (identical? h (certificate/require-current-certificate!
                       h catalog (:current-graph p))))))

(deftest altered-certificate-root-cannot-be-used-to-reissue
  (let [p @valid-package
        other-root (certificate/induction-certificate-subject!
                    (assoc catalog :id :request/another-catalog))
        other-graph {:roots #{(:id other-root)}
                     :nodes [{:id (:id other-root)
                              :subject other-root
                              :subject-handle (admission/admit-subject! other-root)
                              :depends-on []}]}]
    (cert-rejects!
     #(certificate/admit-certificate!
       (assoc p :graph-handle (dependency/check-graph! other-graph)
              :current-graph other-graph))
     :wrong-certificate-roots)))

(deftest frozen-checker-boundary-detects-fabricated-success
  (let [original induction/check-induction!
        good (checked! catalog)]
    (with-redefs [induction/check-induction!
                  (fn [_] (assoc good :authority :production
                                 :checker-soundness :proved))]
      (cert-rejects!
       #(certificate/admit-certificate! @valid-package)
       :induction-checker-contract-changed))))

(deftest sibling-checker-substitution-cannot-authorize-false-induction
  (let [public initial/check-initial-consequence!
        base-report (public {:construction
                             (:construction (first (:operations catalog)))
                             :initial-condition base
                             :invariants invariants})]
    (with-redefs [initial/check-initial-consequence!
                  (fn [_] (assoc base-report :certificate :issued
                                 :authority :production))]
      (let [data (error-data #(checked! catalog))]
        (is (= :gesso.assurance.proof.induction/invalid-induction
               (:error/type data)) (pr-str data))
        (is (= :sibling-checker-contract-changed (:reason data)))))))

(deftest catalog-checker-substitution-cannot-manufacture-coverage
  (let [original operation-set/check-operation-set!
        expected (original catalog)]
    (with-redefs [operation-set/check-operation-set!
                  (fn [_] (assoc expected :external-operation-set-completeness
                                 :proved))]
      (let [data (error-data #(checked! catalog))]
        (is (= :gesso.assurance.proof.induction/invalid-induction
               (:error/type data)))
        (is (= :sibling-checker-contract-changed (:reason data)))))))

(deftest preservation-checker-substitution-cannot-erase-open-step
  (let [c (unsupported-step)
        source {:construction (:construction (second (:operations c)))
                :initial-condition (:initial-condition c)
                :invariants (:invariants c)}
        original preservation/check-unified-preservation!
        expected (original source)]
    (with-redefs [preservation/check-unified-preservation!
                  (fn [_] (assoc expected :authority :production))]
      (let [data (error-data #(checked! c))]
        (is (= :gesso.assurance.proof.induction/invalid-induction
               (:error/type data)))
        (is (= :sibling-checker-contract-changed (:reason data)))))))

(deftest changed-invariant-universe-must-recheck-exact-source
  (let [shrunk (update catalog :invariants dissoc :request/version-nonnegative)
        enlarged (assoc-in catalog [:invariants :request/extra-truth]
                           {:readers #{} :expression [:literal true]})
        good (checked! catalog)
        smaller (checked! shrunk)
        larger (checked! enlarged)]
    (is (:all-declared-invariants-conditionally-inductive? smaller))
    (is (:all-declared-invariants-conditionally-inductive? larger))
    (is (not= (:fingerprint good) (:fingerprint smaller)))
    (is (not= (:fingerprint good) (:fingerprint larger)))
    (let [p @valid-package
          h (certificate/admit-certificate! p)]
      (doseq [changed [shrunk enlarged]]
        (cert-rejects! #(certificate/require-current-certificate!
                         h changed (:current-graph p))
                       :wrong-certificate-subject)))))

(deftest conditionally-checked-is-not-a-missing-proof-discharge
  (let [r (checked! catalog)
        p @valid-package
        d (certificate/describe-certificate (certificate/admit-certificate! p))]
    (is (= :not-independently-proved (:checker-soundness r)))
    (is (= :not-established (:initial-state-existence r)))
    (is (= :not-established (:external-initialization-coverage r)))
    (is (= :not-established (:physical-write-path-coverage r)))
    (is (= :none (:authority r)))
    (is (= :not-performed (:assumption-discharge d)))
    (is (= :not-established (:meta-soundness d)))
    (is (= :not-established (:implementation-correspondence d)))
    (is (= :not-established (:runtime-enforcement d)))
    (is (= :none (:authority d)))
    (is (every? #(contains? (:open-obligations d) %)
                [:checker-meta-soundness :physical-write-path-coverage
                 :implementation-correspondence
                 :external-operation-set-completeness]))))

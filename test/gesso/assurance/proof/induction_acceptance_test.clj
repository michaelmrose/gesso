(ns gesso.assurance.proof.induction-acceptance-test
  "v1287: integrated conditional inductive-safety acceptance checkpoint.

   Exercise a four-operation branching Request lifecycle through original-source
   induction, full source-relative dependency closure, checked certificate
   admission, and the source-bound assurance report. Adversarial tests verify
   that mutations, missing proof steps, forged evidence, and stale handles
   cannot be promoted into checked application-wide conclusions.

   The theorem is about the CLOSED DECLARED ABSTRACT machine only. None of
   these tests establishes checker soundness, physical writer completeness,
   real initializer coverage, XTDB refinement, or runtime authorization."
  (:require [clojure.set :as set]
            [clojure.test :refer [deftest is testing]]
            [gesso.assurance.dependency :as dependency]
            [gesso.assurance.inductive-report :as report]
            [gesso.assurance.proof.certificate :as certificate]
            [gesso.assurance.proof.induction :as induction]
            [gesso.assurance.proof.induction-dependency :as closure]
            [gesso.assurance.proof.induction-ledger :as ledger]
            [gesso.assurance.proof.operation-set :as operation-set]))

(def ^:private state-types
  {:request/status {:type :enum :name :request/status
                    :values #{:open :claimed :completed :cancelled}}
   :request/version {:type :integer}
   :request/priority {:type :enum :name :request/priority
                      :values #{:low :normal :urgent}}
   :request/owner {:type :keyword :nullable? true}})

(def ^:private status-invariant
  {:readers #{:request/status}
   :expression [:in [:read :request/status]
                [:literal #{:open :claimed :completed :cancelled}]]})
(def ^:private version-invariant
  {:readers #{:request/version}
   :expression [:>= [:read :request/version] [:literal 0]]})
(def ^:private priority-invariant
  {:readers #{:request/priority}
   :expression [:in [:read :request/priority]
                [:literal #{:low :normal :urgent}]]})
(def ^:private invariants
  {:request/valid-status status-invariant
   :request/nonnegative-version version-invariant
   :request/valid-priority priority-invariant
   :request/truth {:readers #{} :expression [:literal true]}})
(def ^:private initial
  {:readers #{:request/status :request/version :request/priority}
   :expression [:and (:expression status-invariant)
                (:expression version-invariant)
                (:expression priority-invariant)]})

(defn- operation [id from to]
  {:subject {:kind :operation :id id :interface {:operation id}}
   :state-types state-types
   :input-types {}
   :guard {:readers #{:request/status}
           :expression [:= [:read :request/status] [:literal from]]}
   :reads #{:request/status}
   :writes #{:request/status}
   :replacements {:request/status [:literal to]}
   :frame #{:request/version :request/priority :request/owner}})

(def ^:private operations
  [{:id :request/claim
    :construction (operation :request/claim :open :claimed)}
   {:id :request/unclaim
    :construction (operation :request/unclaim :claimed :open)}
   {:id :request/complete
    :construction (operation :request/complete :claimed :completed)}
   {:id :request/cancel
    :construction (operation :request/cancel :open :cancelled)}])
(def ^:private operation-ids (set (map :id operations)))
(def ^:private invariant-ids (set (keys invariants)))
(def ^:private catalog
  {:id :request/acceptance-workflow
   :declared-operation-ids operation-ids
   :initial-condition initial
   :invariants invariants
   :operations operations})

(def ^:private induction-premises
  #{:gesso.assurance.proof.induction/matching-declared-abstract-machine})
(def ^:private certification-boundaries
  #{:production-transition-correspondence :current-runtime-authorization
    :atomic-transaction-enforcement :checker-meta-soundness})
(def ^:private must-remain-open
  #{:induction-rule-soundness :checker-meta-soundness
    :initial-state-existence :external-initializer-coverage
    :external-operation-set-completeness :physical-write-path-coverage
    :implementation-correspondence})

(defn- source-only [c]
  {:catalog c :certificate-handle nil
   :dependency-handle nil :current-graph nil})

(defn- evidence-package
  "Produce both CURRENT live handles independently from original source.
   The generic DAG handle is separate from the exact induction closure."
  [c]
  (let [graph (closure/build-induction-dependency-graph! c)
        induction-result (induction/check-induction! c)
        deps (closure/check-induction-dependencies! c graph)
        cert (certificate/admit-certificate!
              {:proof-kind :conditional-induction
               :proof-source c
               :graph-handle (dependency/check-graph! graph)
               :current-graph graph
               :expected-open-premises induction-premises
               :expected-open-obligations
               (set/union certification-boundaries
                          (disj (:open-meta-obligations induction-result)
                                :certificate-admission))})]
    {:catalog c :certificate-handle cert
     :dependency-handle deps :current-graph graph}))

(defn- exception-data [thunk]
  (try (thunk) nil
       (catch clojure.lang.ExceptionInfo ex (ex-data ex))))

(defn- rejects! [thunk expected-type expected-reason]
  (let [data (exception-data thunk)]
    (is (map? data) (str "Expected typed failure " expected-reason))
    (is (= expected-type (:error/type data)) (pr-str data))
    (is (= expected-reason (:reason data)) (pr-str data))
    data))

(defn- report-rejects! [thunk reason]
  (rejects! thunk :gesso.assurance.inductive-report/invalid-inductive-report reason))

(defn- certificate-rejects! [thunk reason]
  (rejects! thunk :gesso.assurance.proof.certificate/invalid-certificate reason))

(defn- status [report] (get-in report [:classification :checked-abstract :status]))

(deftest entire-declared-branching-machine-is-conditionally-inductive
  (let [catalog-report (operation-set/check-operation-set! catalog)
        induction-result (induction/check-induction! catalog)
        l (ledger/check-induction-ledger! catalog)]
    (is (= operation-ids (:declared-operation-ids catalog-report)))
    (is (= invariant-ids (:declared-invariant-ids induction-result)))
    (is (= operation-ids (:declared-operation-ids induction-result)))
    (is (= :exact-base-and-all-declared-transitions
           (:proof-rule induction-result)))
    (is (true? (:all-declared-invariants-conditionally-inductive?
                induction-result)))
    (is (= invariant-ids
           (:conditionally-checked-inductive-invariant-ids induction-result)))
    (is (empty? (:unsupported-inductive-invariant-ids induction-result)))
    (is (= (count invariants) (count (:induction-outcomes induction-result))))
    (doseq [outcome (:induction-outcomes induction-result)]
      (is (= :checked-conditional-inference (:initial-status outcome)))
      (is (= :checked-conditional-induction (:status outcome)))
      (is (= operation-ids (:preserving-operation-ids outcome)))
      (is (= #{} (:unsupported-operation-ids outcome))))
    (is (= operation-ids (set (keys (:original-open-obligation-ids-by-operation l)))))
    (is (= (count invariants) (count (:checked-abstract-steps l))))
    (is (empty? (:open-abstract-steps l)))
    (is (= :all-original-obligations-remain-open
           (:original-obligation-status l)))
    (is (set/subset? must-remain-open (:open-meta-obligations l)))
    (is (= :not-performed (:assumption-discharge l)))
    (is (= :none (:authority l)))))

(deftest complete-source-only-report-still-has-no-admission
  (let [r (report/build-inductive-report! (source-only catalog))]
    (is (= report/report-type (:type r)))
    (is (= report/report-version (:version r)))
    (is (= operation-ids (:declared-operation-ids r)))
    (is (= invariant-ids (:declared-invariant-ids r)))
    (is (true? (:all-declared-invariants-conditionally-inductive? r)))
    (is (= :source-only-conditional-abstract-analysis (:verdict r)))
    (is (= :checked-conditional-abstract-induction-evidence (status r)))
    (is (= :not-admitted (get-in r [:classification :certificate :status])))
    (is (= :not-established (:source-dependency-closure r)))
    (is (false? (:full-success? r)))
    (is (= :none (:authority r)))
    (is (= :not-established (:production-certification r)))
    (is (= :not-established (:implementation-correspondence r)))
    (is (= :not-established (:runtime-enforcement r)))))

(deftest exact-source-dependencies-and-conditional-certificate-compose
  (let [p (evidence-package catalog)
        r (report/build-inductive-report! p)
        c (certificate/describe-certificate (:certificate-handle p))
        d (closure/describe-induction-dependencies (:dependency-handle p))]
    (is (= :source-bound-conditional-abstract-safety (:verdict r)))
    (is (= :original-sources-and-both-live-handles-rechecked (:checking r)))
    (is (= :source-bound-conditional-induction (status r)))
    (is (= :checked-exact-source-relative (:source-dependency-closure r)))
    (is (= :in-process-conditional-abstract-certificate
           (get-in r [:classification :certificate :status])))
    (is (= :conditional-induction (:proof-kind c)))
    (is (= :checked-conditional-abstract-induction (:proof-status c)))
    (is (= induction-premises (:open-premises c)))
    (is (= :source-dependencies-structurally-checked (:proof-status d)))
    (is (= :original-proof-source-and-exact-dependency-graph (:checking d)))
    (is (= operation-ids (:operation-ids d)))
    (is (= invariant-ids (:invariant-ids d)))
    (is (= (:catalog-id r) (:catalog-id d)))
    (is (= (get-in r [:identity :induction-fingerprint])
           (:derivation-fingerprint c)))
    (is (= (get-in r [:identity :dependency-fingerprint]) (:fingerprint d)))
    (is (= (get-in r [:identity :certificate-fingerprint]) (:fingerprint c)))
    (is (identical? (:certificate-handle p)
                    (certificate/require-current-certificate!
                     (:certificate-handle p) catalog (:current-graph p))))
    (is (identical? (:dependency-handle p)
                    (closure/require-current-induction-dependencies!
                     (:dependency-handle p) catalog (:current-graph p))))
    (is (= r (report/require-current-inductive-report! r p)))
    (is (false? (:full-success? r)))
    (is (= :none (:transaction-authority r)))
    (is (= :none (:authority r)))))

(deftest complete-ledger-retains-every-open-proof-and-physical-premise
  (let [p (evidence-package catalog)
        r (report/build-inductive-report! p)
        i (induction/check-induction! catalog)
        l (ledger/check-induction-ledger! catalog)
        c (certificate/describe-certificate (:certificate-handle p))
        d (closure/describe-induction-dependencies (:dependency-handle p))
        original (:original-open-obligation-ids-by-operation l)
        classes (get-in r [:classification :open :assumption-classes])
        all-open (get-in r [:classification :open :assumptions])]
    (is (= original (get-in r [:classification :original-obligations :by-operation])))
    (is (= original (get-in d [:original-open-obligation-ids-by-operation])))
    (is (= original (:original-open-obligation-ids-by-operation i)))
    (is (= operation-ids (set (keys original))))
    (is (every? seq (vals original)))
    (is (= :all-original-obligations-remain-open
           (get-in r [:classification :original-obligations :status])))
    (is (set/subset? must-remain-open all-open))
    (is (set/subset? certification-boundaries all-open))
    (is (= all-open (set/union (:open-meta-obligations l)
                               (:open-obligations c)
                               (:open-external-obligations d))))
    (is (= all-open (reduce set/union #{} (vals classes))))
    (is (= (count all-open) (reduce + (map count (vals classes)))))
    (is (= :not-performed (:assumption-discharge r)))
    (is (= :not-independently-proved (get-in r [:classification :open :logical-soundness])))
    (is (= :not-established (get-in r [:classification :open :initialization-coverage])))
    (is (= :not-established (get-in r [:classification :open :physical-operation-coverage])))
    (is (= :not-established (get-in r [:classification :open :implementation-correspondence])))
    (is (= :not-established
           (get-in r [:classification :unsupported :universal-xtdb-correspondence])))))

(deftest unsupported-initial-consequence-keeps-specific-invariant-open
  (let [bad (assoc-in catalog [:invariants :request/owner-present]
                      {:readers #{:request/owner}
                       :expression [:not= [:read :request/owner] [:literal nil]]})
        i (induction/check-induction! bad)
        r (report/build-inductive-report! (source-only bad))]
    (is (false? (:all-declared-invariants-conditionally-inductive? i)))
    (is (contains? (:unsupported-inductive-invariant-ids i)
                   :request/owner-present))
    (is (contains? (get-in r [:classification :open-abstract :invariant-ids])
                   :request/owner-present))
    (is (not= :source-bound-conditional-abstract-safety (:verdict r)))
    (is (= :not-admitted (get-in r [:classification :certificate :status])))
    (is (= :none (:authority r)))
    (certificate-rejects!
     #(certificate/induction-certificate-subject! bad)
     :unsafe-induction-judgment)))

(defn- unsupported-version-write [c]
  (let [index (first (keep-indexed (fn [i op] (when (= :request/complete (:id op)) i))
                                   (:operations c)))]
    (-> c
        (assoc-in [:operations index :construction :input-types]
                  {:input/new-version {:type :integer}})
        (assoc-in [:operations index :construction :reads]
                  #{:request/status :input/new-version})
        (assoc-in [:operations index :construction :writes]
                  #{:request/status :request/version})
        (assoc-in [:operations index :construction :replacements]
                  {:request/status [:literal :completed]
                   :request/version [:read :input/new-version]})
        (assoc-in [:operations index :construction :frame]
                  #{:request/priority :request/owner}))))

(deftest one-bad-transition-blocks-whole-catalog-induction
  (let [bad (unsupported-version-write catalog)
        i (induction/check-induction! bad)
        r (report/build-inductive-report! (source-only bad))
        row (first (filter #(= :request/nonnegative-version (:invariant %))
                           (:induction-outcomes i)))]
    (is (false? (:all-declared-invariants-conditionally-inductive? i)))
    (is (= #{:request/complete} (:unsupported-operation-ids row)))
    (is (= :some-preservation-open (:reason row)))
    (is (contains? (get-in r [:classification :open-abstract :invariant-ids])
                   :request/nonnegative-version))
    (is (= :source-only-conditional-abstract-analysis (:verdict r)))
    (is (= :none (:authority r)))
    (certificate-rejects!
     #(certificate/induction-certificate-subject! bad)
     :unsafe-induction-judgment)))

(deftest incomplete-evidence-package-is-not-treated-as-source-only
  (let [p (evidence-package catalog)]
    (doseq [key [:certificate-handle :dependency-handle :current-graph]]
      (report-rejects! #(report/build-inductive-report! (assoc p key nil))
                       :incomplete-evidence-package))
    (report-rejects! #(report/build-inductive-report!
                      (assoc p :authority :all))
                     :invalid-source-input)
    (report-rejects! #(report/build-inductive-report!
                      (with-meta p {:trusted true}))
                     :invalid-source-input)))

(deftest copyable-descriptions-cannot-replace-live-handles
  (let [p (evidence-package catalog)
        c-copy (certificate/describe-certificate (:certificate-handle p))
        d-copy (closure/describe-induction-dependencies (:dependency-handle p))]
    (is (map? c-copy))
    (is (map? d-copy))
    (rejects! #(report/build-inductive-report!
                (assoc p :certificate-handle c-copy))
              :gesso.assurance.proof.certificate/invalid-certificate
              :unrecognized-certificate)
    (rejects! #(report/build-inductive-report!
                (assoc p :dependency-handle d-copy))
              :gesso.assurance.proof.induction-dependency/invalid-induction-dependencies
              :unrecognized-dependency-handle)))

(deftest exact-closure-rejects-missing-source-operation-node
  (let [p (evidence-package catalog)
        g (:current-graph p)
        altered (update g :nodes
                        #(vec (remove (fn [node]
                                        (= :request/complete
                                           (get-in node [:subject :interface :operation-id])))
                                      %)))]
    (is (= (dec (count (:nodes g))) (count (:nodes altered))))
    (rejects! #(closure/check-induction-dependencies! catalog altered)
              :gesso.assurance.proof.induction-dependency/invalid-induction-dependencies
              :source-dependency-coverage-mismatch)
    (rejects! #(report/build-inductive-report!
                (assoc p :current-graph altered))
              :gesso.assurance.proof.induction-dependency/invalid-induction-dependencies
              :source-dependency-content-mismatch)))

(deftest missing-declared-operation-is-rejected-before-induction
  (let [bad (update catalog :declared-operation-ids disj :request/cancel)]
    (rejects! #(induction/check-induction! bad)
              :gesso.assurance.proof.operation-set/invalid-operation-set
              :operation-id-coverage-mismatch)
    (rejects! #(report/build-inductive-report! (source-only bad))
              :gesso.assurance.proof.operation-set/invalid-operation-set
              :operation-id-coverage-mismatch)))

(deftest smaller-closed-declaration-cannot-certify-omitted-real-writers
  ;; The smaller declared machine is itself valid. This is why the assurance
  ;; report must NOT advertise completeness of physical write paths.
  (let [smaller (-> catalog
                    (update :declared-operation-ids disj :request/cancel)
                    (update :operations
                            (fn [xs] (vec (remove #(= :request/cancel (:id %)) xs)))))
        r (report/build-inductive-report! (source-only smaller))]
    (is (true? (:all-declared-invariants-conditionally-inductive? r)))
    (is (= (disj operation-ids :request/cancel)
           (:declared-operation-ids r)))
    (is (= :closed-declared-abstract-machine-only (:scope r)))
    (is (= :not-established
           (get-in r [:classification :open :physical-operation-coverage])))
    (is (= :not-established (:production-certification r)))
    (is (= :none (:authority r)))))

(deftest stale-source-evidence-cannot-be-reused
  (let [p (evidence-package catalog)
        r (report/build-inductive-report! p)
        altered (assoc-in catalog [:operations 0 :construction :guard :expression]
                          [:= [:read :request/status] [:literal :cancelled]])]
    (is (not= (:fingerprint (induction/check-induction! catalog))
              (:fingerprint (induction/check-induction! altered))))
    (is (not= (:fingerprint r)
              (:fingerprint (report/build-inductive-report! (source-only altered)))))
    (is (map? (exception-data #(report/build-inductive-report!
                               (assoc p :catalog altered)))))
    (report-rejects!
     #(report/require-current-inductive-report!
       (assoc r :full-success? true) p)
     :stale-or-forged-inductive-report)
    (report-rejects!
     #(report/require-current-inductive-report!
       (assoc r :classification
              (assoc (:classification r) :open {})) p)
     :stale-or-forged-inductive-report)))

(deftest changed-initial-condition-or-invariant-invalidates-live-evidence
  (let [p (evidence-package catalog)
        original-fingerprint (:fingerprint (induction/check-induction! catalog))
        initial-changed
        (update-in catalog [:initial-condition :expression]
                   (fn [expr] [:and expr
                               [:= [:read :request/status] [:literal :open]]]))
        invariants-changed
        (assoc-in catalog [:invariants :request/extra-true]
                  {:readers #{} :expression [:literal true]})]
    (doseq [changed [initial-changed invariants-changed]]
      (let [candidate (induction/check-induction! changed)
            source-report (report/build-inductive-report! (source-only changed))]
        (is (not= original-fingerprint (:fingerprint candidate)))
        (is (= :source-only-conditional-abstract-analysis
               (:verdict source-report)))
        (is (map? (exception-data #(report/build-inductive-report!
                                   (assoc p :catalog changed)))))
        (is (= :none (:authority source-report)))))))

(deftest forged-report-fields-cannot-grant-authority
  (let [p (evidence-package catalog)
        r (report/build-inductive-report! p)
        forged (-> r
                   (assoc :full-success? true :authority :production
                          :implementation-correspondence :proved
                          :production-certification :established))]
    (is (= :none (:authority r)))
    (is (false? (:full-success? r)))
    (is (= :not-established (:production-certification r)))
    (report-rejects!
     #(report/require-current-inductive-report! forged p)
     :stale-or-forged-inductive-report)
    (rejects!
     #(report/build-inductive-report! (assoc p :catalog forged))
     :gesso.assurance.proof.operation-set/invalid-operation-set
     :wrong-keys)))

(deftest source-relative-scope-is-not-physical-operation-completeness
  (let [p (evidence-package catalog)
        r (report/build-inductive-report! p)]
    (is (= :closed-declared-abstract-machine-only (:scope r)))
    (is (= :not-established (:physical-operation-coverage
                            (get-in r [:classification :open]))))
    (is (= :not-established
           (get-in r [:classification :unsupported :production-certification])))
    (is (= :not-established
           (get-in r [:classification :unsupported :runtime-authorization])))
    (is (= :none (:transaction-authority r)))
    (is (= :none (:authority r)))))

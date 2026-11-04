(ns gesso.assurance.proof.induction-ledger-test
  "v1285: permanent adversarial tests for source-derived induction evidence
   classification (v1284). Checked restricted inferences must never erase
   original obligations or silently discharge metatheoretic, initialization,
   certification, or physical correspondence assumptions.

   The ledger is a copyable explanation, not an admitted certificate or an
   implementation-refinement theorem. These tests do not prove checker
   soundness or completeness of installed write paths."
  (:require [clojure.edn :as edn]
            [clojure.set :as set]
            [clojure.test :refer [deftest is testing]]
            [gesso.assurance.proof.induction :as induction]
            [gesso.assurance.proof.induction-ledger :as ledger]))

(def ^:private state-types
  {:request/status {:type :enum :name :request/status
                    :values #{:open :claimed :done}}
   :request/version {:type :integer}
   :request/owner {:type :keyword :nullable? true}})

(def ^:private domain-invariant
  {:readers #{:request/status}
   :expression [:in [:read :request/status]
                [:literal #{:open :claimed :done}]]})

(def ^:private version-invariant
  {:readers #{:request/version}
   :expression [:>= [:read :request/version] [:literal 0]]})

(def ^:private true-invariant
  {:readers #{} :expression [:literal true]})

(def ^:private base-invariants
  {:request/domain domain-invariant
   :request/version-nonnegative version-invariant
   :request/truth true-invariant})

(def ^:private initial-condition
  {:readers #{:request/status :request/version}
   :expression [:and (:expression domain-invariant)
                (:expression version-invariant)]})

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
  {:id :request/workflow
   :declared-operation-ids #{:request/claim :request/unclaim}
   :initial-condition initial-condition
   :invariants base-invariants
   :operations [{:id :request/claim
                 :construction (operation :request/claim :open :claimed)}
                {:id :request/unclaim
                 :construction (operation :request/unclaim :claimed :open)}]})

(def ^:private required-open-assumptions
  #{:induction-rule-soundness
    :initial-state-existence
    :external-initializer-coverage
    :external-operation-set-completeness
    :physical-write-path-coverage
    :implementation-correspondence
    :checker-meta-soundness})

(defn- checked! [c] (ledger/check-induction-ledger! c))
(defn- induction! [c] (induction/check-induction! c))

(defn- data-from [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn- ledger-rejection! [f reason]
  (let [data (data-from f)]
    (is (map? data) (str "Expected typed rejection: " reason))
    (is (= :gesso.assurance.proof.induction-ledger/invalid-induction-ledger
           (:error/type data)) (pr-str data))
    (is (= reason (:reason data)) (pr-str data))
    data))

(defn- stale! [prior source]
  (ledger-rejection!
   #(ledger/require-current-induction-ledger! prior source)
   :stale-or-forged-induction-ledger))

(defn- row [r id]
  (first (filter #(= id (:invariant-id %))
                 (concat (:checked-abstract-steps r)
                         (:open-abstract-steps r)))))

(defn- all-step-rows [r]
  (concat (:checked-abstract-steps r) (:open-abstract-steps r)))

(defn- unproved-open-only-catalog []
  (let [i {:readers #{:request/status}
           :expression [:= [:read :request/status] [:literal :open]]}]
    (-> catalog
        (assoc-in [:invariants :request/open-only] i)
        (update-in [:initial-condition :expression]
                   (fn [expr] [:and (:expression i) expr])))))

(defn- unproved-initial-catalog []
  (assoc catalog :initial-condition
         {:readers #{} :expression [:literal true]}))

(defn- classifier []
  ;; This private function is intentionally exercised only for the extension
  ;; boundary: unknown future obligation names must remain unproved. Do NOT
  ;; make the private classifier an application-facing API.
  (var-get (get (ns-interns 'gesso.assurance.proof.induction-ledger)
                'classify-open-meta!)))

(deftest canonical-ledger-is-a-non-authoritative-explanation
  (let [r (checked! catalog)]
    (is (= ledger/ledger-type (:type r)))
    (is (= ledger/ledger-version (:version r)))
    (is (re-matches #"sha256:[0-9a-f]{64}" (:fingerprint r)))
    (is (= :request/workflow (:catalog-id r)))
    (is (= :closed-declared-abstract-machine-only (:scope r)))
    (is (= :original-source-induction-rechecked (:checking r)))
    (is (= :checked-conditional-abstract-induction-evidence (:proof-status r)))
    (is (true? (:all-declared-invariants-conditionally-inductive? r)))
    (is (= :not-performed (:assumption-discharge r)))
    (is (= :all-original-obligations-remain-open
           (:original-obligation-status r)))
    (is (= :not-established-by-this-ledger
           (:source-dependency-closure r)))
    (is (= :not-issued (:certificate r)))
    (is (= :not-admitted (:admission r)))
    (is (= :not-established (:runtime-enforcement r)))
    (is (= :not-established (:implementation-correspondence r)))
    (is (= :none (:authority r)))))

(deftest every-invariant-and-operation-step-is-explicit
  (let [r (checked! catalog)
        rows (:checked-abstract-steps r)
        operations (:declared-operation-ids catalog)]
    (is (= #{} (set (map :invariant-id (:open-abstract-steps r)))))
    (is (= (set (keys base-invariants)) (set (map :invariant-id rows))))
    (is (= (count base-invariants) (count rows)))
    (doseq [entry rows]
      (is (= :checked-conditional-abstract (:step-class entry)))
      (is (= :checked-conditional-induction (:induction-status entry)))
      (is (nil? (:open-reason entry)))
      (is (= [:initial-state (:invariant-id entry)]
             (get-in entry [:initial :obligation-id])))
      (is (= :checked-conditional-inference
             (get-in entry [:initial :status])))
      (is (some? (get-in entry [:initial :rule])))
      (is (= operations (set (keys (:preservation-by-operation entry)))))
      (doseq [[operation-id p] (:preservation-by-operation entry)]
        (is (contains? operations operation-id))
        (is (= [:preservation (:invariant-id entry)] (:obligation-id p)))
        (is (= :checked-conditional-inference (:status p)))))))

(deftest ledger-binds-to-original-induction-source
  (let [r (checked! catalog)
        source (induction! catalog)]
    (is (= (:fingerprint source) (:induction-fingerprint r)))
    (is (= (:catalog-fingerprint source) (:catalog-fingerprint r)))
    (is (= (:declared-operation-ids source) (:declared-operation-ids r)))
    (is (= (:declared-invariant-ids source) (:declared-invariant-ids r)))
    (is (= (set (map :invariant-id (all-step-rows r)))
           (:declared-invariant-ids r)))
    (is (= (:all-declared-invariants-conditionally-inductive? source)
           (:all-declared-invariants-conditionally-inductive? r)))))

(deftest all-original-operation-obligations-survive-successful-checks
  (let [r (checked! catalog)
        source (induction! catalog)
        actual (:original-open-obligation-ids-by-operation r)]
    (is (= actual (:original-open-obligation-ids-by-operation source)))
    (is (= (:declared-operation-ids r) (set (keys actual))))
    (doseq [[id obligations] actual]
      (is (contains? (:declared-operation-ids r) id))
      (is (set? obligations))
      (is (seq obligations))
      (doseq [invariant (:declared-invariant-ids r)]
        (is (contains? obligations [:initial-state invariant]))
        (is (contains? obligations [:preservation invariant]))))
    (is (= :all-original-obligations-remain-open
           (:original-obligation-status r)))))

(deftest classification-is-exhaustive-disjoint-and-still-open
  (let [r (checked! catalog)
        groups (:open-assumptions r)
        all (reduce set/union #{} (vals groups))]
    (is (= #{:logical-soundness :initialization :abstract-semantics
             :physical-correspondence :certificate-admission :unclassified}
           (set (keys groups))))
    (is (= all (:open-meta-obligations r)))
    (is (= (count all) (reduce + (map count (vals groups)))))
    (is (set/subset? required-open-assumptions all))
    (is (every? set? (vals groups)))
    (is (= :not-performed (:assumption-discharge r)))
    (is (contains? (:logical-soundness groups) :checker-meta-soundness))
    (is (contains? (:logical-soundness groups) :induction-rule-soundness))
    (is (contains? (:initialization groups) :initial-state-existence))
    (is (contains? (:initialization groups) :external-initializer-coverage))
    (is (contains? (:physical-correspondence groups)
                   :external-operation-set-completeness))
    (is (contains? (:physical-correspondence groups)
                   :physical-write-path-coverage))
    (is (contains? (:physical-correspondence groups)
                   :implementation-correspondence))
    (is (contains? (:certificate-admission groups)
                   :certificate-admission))))

(deftest logical-and-physical-boundaries-must-not-be-conflated
  (let [r (checked! catalog)
        logical (get-in r [:open-assumptions :logical-soundness])
        physical (get-in r [:open-assumptions :physical-correspondence])]
    (is (empty? (set/intersection logical physical)))
    (is (not (contains? logical :physical-write-path-coverage)))
    (is (not (contains? physical :checker-meta-soundness)))
    (is (= :not-established (:implementation-correspondence r)))
    (is (= :not-established (:runtime-enforcement r)))
    (is (= :none (:authority r)))))

(deftest checked-evidence-is-not-discharge-of-the-original-obligation
  (let [r (checked! catalog)
        checked-id (get-in (row r :request/domain) [:initial :obligation-id])
        checked-pres (get-in (row r :request/domain)
                             [:preservation-by-operation :request/claim
                              :obligation-id])]
    (is (= :checked-conditional-inference
           (get-in (row r :request/domain) [:initial :status])))
    (is (= :checked-conditional-inference
           (get-in (row r :request/domain)
                   [:preservation-by-operation :request/claim :status])))
    (is (every? #(contains? % checked-id)
                (vals (:original-open-obligation-ids-by-operation r))))
    (is (every? #(contains? % checked-pres)
                (vals (:original-open-obligation-ids-by-operation r))))
    (is (= :all-original-obligations-remain-open
           (:original-obligation-status r)))))

(deftest open-initial-cases-never-disappear-into-successful-transitions
  (let [r (checked! (unproved-initial-catalog))]
    (is (false? (:all-declared-invariants-conditionally-inductive? r)))
    (is (= :partial-conditional-induction-evidence (:proof-status r)))
    (is (seq (:open-abstract-steps r)))
    (is (some #(= :initial-consequence-open (:open-reason %))
              (:open-abstract-steps r)))
    (doseq [entry (:open-abstract-steps r)]
      (is (= :open-abstract-inference (:step-class entry)))
      (is (= :open (:induction-status entry)))
      (when (= :initial-consequence-open (:open-reason entry))
        (is (= :open (get-in entry [:initial :status])))))
    (is (= :all-original-obligations-remain-open
           (:original-obligation-status r)))
    (is (set/subset? required-open-assumptions (:open-meta-obligations r)))
    (is (= :none (:authority r)))))

(deftest one-open-preservation-keeps-its-invariant-open
  (let [r (checked! (unproved-open-only-catalog))
        suspect (row r :request/open-only)]
    (is (= :partial-conditional-induction-evidence (:proof-status r)))
    (is (= :open (:induction-status suspect)))
    (is (= :open-abstract-inference (:step-class suspect)))
    (is (= :some-preservation-open (:open-reason suspect)))
    (is (= :checked-conditional-inference (get-in suspect [:initial :status])))
    (is (= :open (get-in suspect
                         [:preservation-by-operation :request/claim :status])))
    (is (contains? (get-in r [:original-open-obligation-ids-by-operation
                               :request/claim])
                   [:preservation :request/open-only]))
    (is (= :checked-conditional-induction
           (:induction-status (row r :request/domain))))
    (is (= :none (:authority r)))))

(deftest a-partial-catalog-retains-all-original-obligations
  (let [c (unproved-open-only-catalog)
        r (checked! c)
        initial (induction! c)]
    (is (= (:original-open-obligation-ids-by-operation initial)
           (:original-open-obligation-ids-by-operation r)))
    (is (contains? (:open-meta-obligations r) :checker-meta-soundness))
    (is (contains? (:open-meta-obligations r) :implementation-correspondence))
    (is (contains? (:open-meta-obligations r) :external-operation-set-completeness))
    (is (some #(= :checked-conditional-induction (:induction-status %))
              (:checked-abstract-steps r)))
    (is (some #(= :open (:induction-status %))
              (:open-abstract-steps r)))
    (is (= :not-admitted (:admission r)))))

(deftest unknown-future-assumptions-are-kept-as-open
  (let [unknown #{:future/unknown-proof-rule :future/new-runtime-boundary}
        all (set/union unknown #{:checker-meta-soundness
                                 :initial-state-existence})
        groups ((classifier) all)]
    (is (= unknown (:unclassified groups)))
    (is (= #{:checker-meta-soundness} (:logical-soundness groups)))
    (is (= #{:initial-state-existence} (:initialization groups)))
    (is (= all (reduce set/union #{} (vals groups))))
    (is (= (count all) (reduce + (map count (vals groups)))))))

(deftest empty-assumption-classification-does-not-invent-proofs
  (let [groups ((classifier) #{})]
    (is (= #{} (reduce set/union #{} (vals groups))))
    (is (every? empty? (vals groups)))
    (is (contains? groups :unclassified))))

(deftest unknown-assumptions-cannot-be-misfiled-as-discharged
  (let [unknown #{:hypothetical/atomicity :hypothetical/refinement}
        groups ((classifier) (conj unknown :certificate-admission))]
    (is (= unknown (:unclassified groups)))
    (is (= #{:certificate-admission} (:certificate-admission groups)))
    (is (= :not-performed (:assumption-discharge (checked! catalog))))))

(deftest rechecking-a-current-ledger-produces-the-same-explanation
  (let [prior (checked! catalog)
        current (ledger/require-current-induction-ledger! prior catalog)]
    (is (= prior current))
    (is (= prior (checked! catalog)))
    (is (not (identical? prior current)))
    (is (= (:fingerprint prior) (:fingerprint current)))
    (is (= :none (:authority current)))))

(deftest edn-roundtrip-of-copyable-ledger-adds-no-authority
  (let [prior (checked! catalog)
        copied (edn/read-string (pr-str prior))]
    (is (= prior copied))
    (is (= prior (ledger/require-current-induction-ledger! copied catalog)))
    (is (= :not-issued (:certificate copied)))
    (is (= :none (:authority copied)))))

(deftest modified-copy-cannot-pass-freshness
  (let [r (checked! catalog)]
    (doseq [[path replacement] [[[:authority] :runtime-authorized]
                                [[:certificate] :issued]
                                [[:admission] :admitted]
                                [[:assumption-discharge] :completed]
                                [[:original-obligation-status] :all-discharged]
                                [[:implementation-correspondence] :proved]
                                [[:runtime-enforcement] :verified]
                                [[:proof-status] :universally-proved]
                                [[:open-meta-obligations] #{}]
                                [[:open-assumptions] {}]
                                [[:checked-abstract-steps] []]
                                [[:open-abstract-steps] [{}]]
                                [[:source-dependency-closure] :complete]
                                [[:fingerprint] "sha256:0000"]]]
      (testing (pr-str path)
        (stale! (assoc-in r path replacement) catalog)))))

(deftest adding-a-fabricated-proof-field-is-rejected
  (let [r (checked! catalog)]
    (stale! (assoc r :independently-verified? true) catalog)
    (stale! (assoc r :runtime-write-path-coverage :complete) catalog)
    (stale! (assoc r :all-obligations-discharged? true) catalog)))

(deftest forged-step-evidence-is-rejected
  (let [r (checked! catalog)]
    (stale! (assoc-in r [:checked-abstract-steps 0 :induction-status]
                      :proved) catalog)
    (stale! (assoc-in r [:checked-abstract-steps 0 :initial :status]
                      :proved) catalog)
    (stale! (assoc-in r [:checked-abstract-steps 0
                        :preservation-by-operation :request/claim :status]
                      :proved) catalog)))

(deftest omitting-one-original-obligation-is-rejected
  (let [r (checked! catalog)
        operation-id :request/claim
        to-drop [:initial-state :request/domain]
        tampered (update-in r [:original-open-obligation-ids-by-operation
                               operation-id] disj to-drop)]
    (is (contains? (get-in r [:original-open-obligation-ids-by-operation
                               operation-id]) to-drop))
    (stale! tampered catalog)))

(deftest dropping-one-physical-premise-is-rejected
  (let [r (checked! catalog)
        missing :physical-write-path-coverage]
    (stale! (update r :open-meta-obligations disj missing) catalog)
    (stale! (update-in r [:open-assumptions :physical-correspondence]
                       disj missing) catalog)))

(deftest hiding-an-unknown-classification-is-rejected
  (let [r (checked! catalog)]
    (stale! (assoc-in r [:open-assumptions :unclassified]
                      #{:fictional/discharged}) catalog)))

(deftest metadata-cannot-promote-a-copied-ledger
  (let [r (checked! catalog)]
    (stale! (with-meta r {:proof :complete}) catalog)
    (stale! (with-meta (into {} r) {:trusted? true}) catalog)))

(deftest graph-closure-cannot-be-synthesized-by-a-ledger-field
  (let [r (checked! catalog)]
    (is (= :not-established-by-this-ledger (:source-dependency-closure r)))
    (stale! (assoc r :source-dependency-closure :exact) catalog)
    (is (= :none (:authority r)))))

(deftest changing-a-valid-operation-guard-invalidates-prior-evidence
  (let [r (checked! catalog)
        changed (assoc-in catalog
                          [:operations 0 :construction :guard :expression]
                          [:= [:read :request/status] [:literal :done]])]
    (is (not= (:induction-fingerprint r)
              (:induction-fingerprint (checked! changed))))
    (stale! r changed)))

(deftest changing-a-valid-transition-output-invalidates-prior-evidence
  (let [r (checked! catalog)
        changed (assoc-in catalog
                          [:operations 0 :construction :replacements
                           :request/status]
                          [:literal :done])]
    (stale! r changed)))

(deftest changing-the-initial-condition-invalidates-prior-evidence
  (let [r (checked! catalog)]
    (stale! r (unproved-initial-catalog))))

(deftest changing-invariant-source-invalidates-prior-evidence
  (let [r (checked! catalog)
        changed (assoc-in catalog [:invariants :request/version-nonnegative]
                          {:readers #{:request/version}
                           :expression [:>= [:read :request/version]
                                        [:literal 1]]})]
    (stale! r changed)))

(deftest changing-the-catalog-subject-invalidates-prior-evidence
  (let [r (checked! catalog)]
    (stale! r (assoc catalog :id :request/workflow-v2))))

(deftest changed-operation-catalog-membership-invalidates-prior-evidence
  (let [r (checked! catalog)
        changed (-> catalog
                    (assoc :declared-operation-ids #{:request/claim})
                    (assoc :operations [(first (:operations catalog))]))]
    (stale! r changed)))

(deftest operation-reordering-does-not-suppress-open-obligations
  (let [r (checked! catalog)
        reordered (assoc catalog :operations (vec (reverse (:operations catalog))))
        current (checked! reordered)]
    (is (= r current))
    (is (= (:original-open-obligation-ids-by-operation r)
           (:original-open-obligation-ids-by-operation current)))
    (is (= (:open-assumptions r) (:open-assumptions current)))
    (is (= r (ledger/require-current-induction-ledger! r reordered)))))

(deftest source-reports-are-not-accepted-as-original-catalogs
  (let [r (checked! catalog)
        source-report (induction! catalog)]
    (is (map? (data-from #(checked! r))))
    (is (map? (data-from #(checked! source-report))))))

(deftest substituted-checker-with-fabricated-success-is-rejected
  (let [original induction/check-induction!]
    (ledger-rejection!
     #(with-redefs [induction/check-induction!
                    (fn [c] (assoc (original c)
                                   :authority :verified-and-authorized))]
        (checked! catalog))
     :induction-checker-contract-changed)))

(deftest substituted-checker-cannot-hide-open-assumptions
  (let [original induction/check-induction!]
    (ledger-rejection!
     #(with-redefs [induction/check-induction!
                    (fn [c] (assoc (original c)
                                   :open-meta-obligations #{}))]
        (checked! catalog))
     :induction-checker-contract-changed)))

(deftest substituted-checker-cannot-rewrite-original-obligation-register
  (let [original induction/check-induction!]
    (ledger-rejection!
     #(with-redefs [induction/check-induction!
                    (fn [c]
                      (assoc (original c)
                             :original-open-obligation-ids-by-operation {}))]
        (checked! catalog))
     :induction-checker-contract-changed)))

(deftest substituted-checker-cannot-create-a-false-checked-invariant
  (let [c (unproved-open-only-catalog)
        original induction/check-induction!]
    (ledger-rejection!
     #(with-redefs [induction/check-induction!
                    (fn [source]
                      (assoc (original source)
                             :all-declared-invariants-conditionally-inductive?
                             true))]
        (checked! c))
     :induction-checker-contract-changed)))

(deftest unchanged-wrapper-cannot-create-more-authority
  (let [original induction/check-induction!
        report (with-redefs [induction/check-induction!
                             (fn [c] (original c))]
                 (checked! catalog))]
    (is (= (checked! catalog) report))
    (is (= :none (:authority report)))))

(deftest malformed-or-incomplete-catalogs-do-not-produce-ledgers
  (let [invalids [(dissoc catalog :initial-condition)
                  (dissoc catalog :declared-operation-ids)
                  (assoc catalog :operations [])
                  (assoc catalog :invariants {})
                  (assoc catalog :operations [(first (:operations catalog))])]]
    (doseq [bad invalids]
      (is (map? (data-from #(checked! bad)))))))

(deftest induction-evidence-remains-distinct-from-certificate-admission
  (let [r (checked! catalog)]
    (is (= :checked-conditional-abstract-induction-evidence (:proof-status r)))
    (is (= :not-issued (:certificate r)))
    (is (= :not-admitted (:admission r)))
    (is (= :none (:authority r)))
    (is (contains? (:open-meta-obligations r) :certificate-admission))))

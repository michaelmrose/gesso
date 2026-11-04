(ns gesso.assurance.proof.induction-test
  "v1279 source-derived conditional induction adversarial tests.

   These are TESTS of a restricted abstract argument, not a soundness proof
   for its checker and not physical XTDB, authorization, or browser refinement.
   The original v1278 smoke cases are retained and strengthened; neither
   report fingerprints nor caller flags confer certificates or authority."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.set :as set]
            [gesso.assurance.proof.induction :as induction]
            [gesso.assurance.proof.initial :as initial]
            [gesso.assurance.proof.unified-preservation :as preservation]
            [gesso.assurance.proof.operation-set :as operation-set]
            [gesso.assurance.proof.obligation :as obligation]))

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
(defn- check! [c] (induction/check-induction! c))
(defn- err [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))
(defn- row [r id] (first (filter #(= id (:invariant %)) (:induction-outcomes r))))

(deftest full-conditional-induction-across-two-declared-operations
  (let [r (check! catalog)]
    (is (= induction/report-type (:type r)))
    (is (= #{:request/domain :request/version-nonnegative :request/truth}
           (:conditionally-checked-inductive-invariant-ids r)))
    (is (empty? (:unsupported-inductive-invariant-ids r)))
    (is (:all-declared-invariants-conditionally-inductive? r))
    (is (= #{:request/claim :request/unclaim}
           (:preserving-operation-ids (row r :request/domain))))
    (is (= :checked-conditional-inference (:initial-status (row r :request/domain))))
    (is (= :checked-conditional-induction (:status (row r :request/domain))))
    (is (= :not-issued (:certificate r)))
    (is (= :not-admitted (:admission r)))
    (is (= :none (:authority r)))
    (is (= :not-established (:physical-write-path-coverage r)))
    (is (= :not-independently-proved (:checker-soundness r)))
    (is (= 2 (count (:original-open-obligation-ids-by-operation r))))))

(deftest unsupported-base-keeps-only-that-invariant-open
  (let [c (assoc catalog :invariants
                 (assoc base-invariants
                        :request/owner-non-null
                        {:readers #{:request/owner}
                         :expression [:not= [:read :request/owner]
                                      [:literal nil]]}))
        r (check! c)]
    (is (= #{:request/owner-non-null}
           (:unsupported-inductive-invariant-ids r)))
    (is (= :initial-consequence-open
           (:reason (row r :request/owner-non-null))))
    (is (= :open (:status (row r :request/owner-non-null))))
    (is (= :checked-conditional-induction
           (:status (row r :request/domain))))))

(deftest unsupported-transition-keeps-otherwise-established-invariant-open
  (let [bad (operation :request/unclaim :claimed :done)
        bad (-> bad (assoc :input-types {:input/new-version {:type :integer}})
                (assoc :reads #{:request/status :input/new-version})
                (assoc :writes #{:request/status :request/version})
                (assoc :replacements {:request/status [:literal :done]
                                      :request/version [:read :input/new-version]})
                (assoc :frame #{:request/owner}))
        c (assoc catalog :operations
                 [{:id :request/claim :construction claim}
                  {:id :request/unclaim :construction bad}])
        r (check! c)]
    (is (= :checked-conditional-inference (:initial-status (row r :request/version-nonnegative))))
    (is (= #{:request/unclaim}
           (:unsupported-operation-ids (row r :request/version-nonnegative))))
    (is (= :some-preservation-open
           (:reason (row r :request/version-nonnegative))))
    (is (= :checked-conditional-induction (:status (row r :request/domain))))
    (is (not (:all-declared-invariants-conditionally-inductive? r)))))

(deftest order-invariant-and-source-fresh
  (let [r (check! catalog)
        reordered (assoc catalog :operations (vec (reverse (:operations catalog))))
        changed (assoc catalog :operations
                       [{:id :request/claim :construction
                         (assoc-in claim [:subject :interface :operation] :request/claim-v2)}
                        {:id :request/unclaim :construction unclaim}])]
    (is (= r (check! reordered)))
    (is (not= (:fingerprint r) (:fingerprint (check! changed))))))

(deftest deleting-one-declared-member-is-rejected
  (let [c (assoc catalog :operations [ (first (:operations catalog)) ])
        e (err #(check! c))]
    (is (= :gesso.assurance.proof.operation-set/invalid-operation-set
           (:error/type e)))
    (is (= :operation-id-coverage-mismatch (:reason e)))))

(deftest self-consistent-forgeries-of-sibling-reports-rejected
  (let [real initial/check-initial-consequence!
        no-sound (fn [source]
                   (let [r (real source)]
                     (assoc r :conditionally-checked-initial-ids
                            #{[:initial-state :request/domain]})))]
    (with-redefs [initial/check-initial-consequence! no-sound]
      (is (= :sibling-checker-contract-changed
             (:reason (err #(check! catalog)))))))
  (let [real preservation/check-unified-preservation!]
    (with-redefs [preservation/check-unified-preservation!
                  (fn [s]
                    (-> (real s)
                        (assoc :authority :all-powerful)))]
      (is (= :sibling-checker-contract-changed
             (:reason (err #(check! catalog))))))))

(deftest public-operation-catalog-injection-rejected
  (let [real operation-set/check-operation-set!]
    (with-redefs [operation-set/check-operation-set!
                  (fn [c] (assoc (real c) :exact-declared-operation-coverage :checked-and-certified))]
      (is (= :sibling-checker-contract-changed
             (:reason (err #(check! catalog))))))))

(deftest no-post-facto-authority-even-when-all-conditional-inferences-checked
  (let [r (check! catalog)]
    (is (= :not-established (:external-initialization-coverage r)))
    (is (= :not-established (:external-operation-set-completeness r)))
    (is (contains? (:open-meta-obligations r) :induction-rule-soundness))
    (is (contains? (:open-meta-obligations r) :implementation-correspondence))
    (is (= :restricted-conditional-induction-candidates-checked
           (:proof-status r)))))

(deftest impossible-literal-initial-condition-gets-no-induction-success
  (let [c (assoc catalog :initial-condition
                 {:readers #{} :expression [:literal false]})
        r (check! c)]
    (is (empty? (:conditionally-checked-inductive-invariant-ids r)))
    (is (= (set (keys base-invariants))
           (:unsupported-inductive-invariant-ids r)))
    (is (every? #(= :initial-consequence-open (:reason %))
                (:induction-outcomes r)))
    (is (= :not-established (:initial-state-existence r)))))

(deftest weak-initial-condition-does-not-fabricate-the-base-case
  (let [r (check! (assoc catalog :initial-condition
                         {:readers #{} :expression [:literal true]}))]
    (is (= #{:request/truth}
           (:conditionally-checked-inductive-invariant-ids r)))
    (is (= #{:request/domain :request/version-nonnegative}
           (:unsupported-inductive-invariant-ids r)))))

(deftest shrinking-declared-invariant-universe-does-not-certify-original
  (let [r (check! catalog)
        c (update catalog :invariants dissoc :request/domain)
        narrower (check! c)]
    (is (not= (:fingerprint r) (:fingerprint narrower)))
    (is (= (set (keys (:invariants c)))
           (:declared-invariant-ids narrower)))
    (is (not (contains? (:conditionally-checked-inductive-invariant-ids narrower)
                        :request/domain)))
    (is (= :not-established (:external-operation-set-completeness narrower)))
    (is (= :none (:authority narrower)))))

(deftest one-transition-breaking-otherwise-true-postcondition-keeps-open
  (let [not-done {:readers #{:request/status}
                  :expression [:not= [:read :request/status] [:literal :done]]}
        c (assoc catalog
                 :initial-condition
                 {:readers #{:request/status :request/version}
                  :expression [:and (:expression not-done)
                               (:expression invariant-version)]}
                 :invariants {:request/not-done not-done
                              :request/version-nonnegative invariant-version})
        c (update c :declared-operation-ids conj :request/complete)
        c (update c :operations conj
                  {:id :request/complete
                   :construction (operation :request/complete :claimed :done)})
        r (check! c)]
    (is (= :checked-conditional-inference
           (:initial-status (row r :request/not-done))))
    (is (contains? (:unsupported-operation-ids (row r :request/not-done))
                   :request/complete))
    (is (= :open (:status (row r :request/not-done))))
    (is (= :checked-conditional-induction
           (:status (row r :request/version-nonnegative))))))

(deftest closed-relative-to-declared-catalog-even-if-member-deliberately-dropped
  (let [smaller (-> catalog
                    (assoc :declared-operation-ids #{:request/claim})
                    (assoc :operations [{:id :request/claim :construction claim}]))
        r (check! smaller)]
    (is (= #{:request/claim} (:declared-operation-ids r)))
    (is (:all-declared-invariants-conditionally-inductive? r))
    (is (not= (:fingerprint r) (:fingerprint (check! catalog))))
    (is (= :not-established (:external-operation-set-completeness r)))
    (is (= :not-established (:physical-write-path-coverage r)))))


;; Exact identities, accounting, and meaningful counterexamples. Upstream
;; rejections are asserted at their owning boundary, not widened to exceptions.
(def ^:private catalog-error
  :gesso.assurance.proof.operation-set/invalid-operation-set)
(def ^:private induction-error
  :gesso.assurance.proof.induction/invalid-induction)
(def ^:private construction-error
  :gesso.assurance.transition/invalid-construction)

(defn- rejects! [c error-type reason]
  (let [e (err #(check! c))]
    (is (= error-type (:error/type e)) (pr-str e))
    (is (= reason (:reason e)) (pr-str e))
    e))

(defn- change-operation [c id f]
  (update c :operations
          (fn [xs]
            (mapv (fn [x]
                    (if (= id (:id x))
                      (update x :construction f)
                      x))
                  xs))))

(defn- add-operation [c id original]
  (-> c
      (update :declared-operation-ids conj id)
      (update :operations conj
              {:id id
               :construction (-> original
                                 (assoc-in [:subject :id] id)
                                 (assoc-in [:subject :interface :operation] id))})))

(defn- sha256? [x]
  (and (string? x) (boolean (re-matches #"sha256:[0-9a-f]{64}" x))))

(deftest exact-complete-report-schema-remains-conditional
  (let [r (check! catalog)
        ops #{:request/claim :request/unclaim}
        inv (set (keys base-invariants))]
    (is (= induction/report-type (:type r)))
    (is (= induction/induction-version (:version r)))
    (is (sha256? (:fingerprint r)))
    (is (sha256? (:catalog-fingerprint r)))
    (is (= (:fingerprint (operation-set/check-operation-set! catalog))
           (:catalog-fingerprint r)))
    (is (= :request/workflow (:catalog-id r)))
    (is (= ops (:declared-operation-ids r)))
    (is (= inv (:declared-invariant-ids r)))
    (is (= inv (:conditionally-checked-inductive-invariant-ids r)))
    (is (empty? (:unsupported-inductive-invariant-ids r)))
    (is (true? (:all-declared-invariants-conditionally-inductive? r)))
    (is (= (vec (sort-by str inv))
           (mapv :invariant (:induction-outcomes r))))
    (doseq [o (:induction-outcomes r)]
      (is (= :checked-conditional-induction (:status o)))
      (is (= :checked-conditional-inference (:initial-status o)))
      (is (= [:initial-state (:invariant o)] (:initial-obligation-id o)))
      (is (= [:preservation (:invariant o)] (:preservation-obligation-id o)))
      (is (= ops (:preserving-operation-ids o)))
      (is (= #{} (:unsupported-operation-ids o)))
      (is (nil? (:reason o))))
    (is (= :exact-base-and-all-declared-transitions (:proof-rule r)))
    (is (= :closed-declared-abstract-machine-only (:scope r)))
    (is (= :restricted-conditional-induction-candidates-checked (:proof-status r)))
    (is (= :not-independently-proved (:checker-soundness r)))
    (is (= :not-established (:initial-state-existence r)))
    (is (= :not-established (:external-initialization-coverage r)))
    (is (= :not-established (:external-operation-set-completeness r)))
    (is (= :not-established (:physical-write-path-coverage r)))
    (is (= :not-established (:runtime-enforcement r)))
    (is (= :not-established (:implementation-correspondence r)))
    (is (= :not-issued (:certificate r)))
    (is (= :not-admitted (:admission r)))
    (is (= :none (:authority r)))
    (is (set/subset? #{:induction-rule-soundness
                       :initial-state-existence
                       :external-initializer-coverage
                       :external-operation-set-completeness
                       :physical-write-path-coverage
                       :implementation-correspondence
                       :certificate-admission}
                     (:open-meta-obligations r)))))

(deftest operation-evidence-rechecks-exact-original-obligations
  (let [r (check! catalog)
        evidence (:operation-evidence r)
        ops #{:request/claim :request/unclaim}
        inv (set (keys base-invariants))]
    (is (= (vec (sort-by str ops)) (mapv :operation-id evidence)))
    (is (= ops (set (keys (:original-open-obligation-ids-by-operation r)))))
    (doseq [{:keys [operation-id original-open-obligation-ids] :as entry} evidence]
      (let [source {:construction (get-in (first (filter #(= operation-id (:id %))
                                                      (:operations catalog)))
                                         [:construction])
                    :initial-condition (:initial-condition catalog)
                    :invariants (:invariants catalog)}
            ledger (obligation/derive-obligations! source)
            original (:open-obligation-ids ledger)]
        (is (sha256? (:obligation-fingerprint entry)))
        (is (sha256? (:initial-fingerprint entry)))
        (is (sha256? (:preservation-fingerprint entry)))
        (is (= (:fingerprint ledger) (:obligation-fingerprint entry)))
        (is (= (:transition-fingerprint ledger) (:transition-fingerprint entry)))
        (is (= (:open-obligation-ids ledger) original-open-obligation-ids))
        (is (= original (get (:original-open-obligation-ids-by-operation r)
                             operation-id)))
        (is (set/subset? (set (for [id inv] [:initial-state id])) original))
        (is (set/subset? (set (for [id inv] [:preservation id])) original))
        (is (contains? original [:assumption :initial-state-existence]))
        (is (contains? original [:assumption :initial-state-coverage]))
        (is (contains? original [:assumption :checker-meta-soundness]))
        (is (contains? original [:implementation :physical-effect-containment]))
        (is (not (contains? entry :discharged-obligation-ids)))))))

(deftest base-case-and-step-case-are-independent
  (let [weak (check! (assoc catalog :initial-condition
                            {:readers #{} :expression [:literal true]}))
        owner (check! (update catalog :invariants assoc
                              :request/owner-not-null
                              {:readers #{:request/owner}
                               :expression [:not= [:read :request/owner]
                                                  [:literal nil]]}))]
    (is (= #{:request/truth}
           (:conditionally-checked-inductive-invariant-ids weak)))
    (is (= #{:request/domain :request/version-nonnegative}
           (:unsupported-inductive-invariant-ids weak)))
    (doseq [id (:unsupported-inductive-invariant-ids weak)]
      (is (= :initial-consequence-open (:reason (row weak id))))
      (is (= :open (:initial-status (row weak id)))))
    (is (= #{:request/owner-not-null}
           (:unsupported-inductive-invariant-ids owner)))
    (is (= :initial-consequence-open
           (:reason (row owner :request/owner-not-null))))
    (is (= :checked-conditional-induction
           (:status (row owner :request/domain))))))

(deftest explicit-impossible-base-is-not-a-vacuous-certificate
  (doseq [initial-rule [{:readers #{} :expression [:literal false]}
                        {:readers #{:request/status}
                         :expression [:and
                                      [:literal false]
                                      (:expression invariant-domain)]}]]
    (let [r (check! (assoc catalog :initial-condition initial-rule))]
      (is (= #{} (:conditionally-checked-inductive-invariant-ids r)))
      (is (= (set (keys base-invariants))
             (:unsupported-inductive-invariant-ids r)))
      (is (every? #(= :initial-consequence-open (:reason %))
                  (:induction-outcomes r)))
      (is (= :not-established (:initial-state-existence r)))
      (is (= :not-issued (:certificate r))))))

(deftest unsupported-preservation-names-the-exact-offending-operation
  (let [broken (-> (operation :request/unclaim :claimed :done)
                   (assoc :input-types {:input/new-version {:type :integer}}
                          :reads #{:request/status :input/new-version}
                          :writes #{:request/status :request/version}
                          :replacements {:request/status [:literal :done]
                                         :request/version [:read :input/new-version]}
                          :frame #{:request/owner}))
        c (assoc catalog :operations
                 [{:id :request/claim :construction claim}
                  {:id :request/unclaim :construction broken}])
        r (check! c)
        o (row r :request/version-nonnegative)]
    (is (= :checked-conditional-inference (:initial-status o)))
    (is (= :open (:status o)))
    (is (= :some-preservation-open (:reason o)))
    (is (= #{:request/claim} (:preserving-operation-ids o)))
    (is (= #{:request/unclaim} (:unsupported-operation-ids o)))
    (is (= #{:request/domain :request/truth}
           (:conditionally-checked-inductive-invariant-ids r)))
    (is (= #{:request/version-nonnegative}
           (:unsupported-inductive-invariant-ids r)))
    (is (= :none (:authority r)))))

(deftest one-of-three-operations-can-break-the-preservation-set
  (let [not-done {:readers #{:request/status}
                  :expression [:not= [:read :request/status] [:literal :done]]}
        c (-> catalog
              (assoc :initial-condition {:readers #{:request/status :request/version}
                                         :expression [:and (:expression not-done)
                                                      (:expression invariant-version)]}
                     :invariants {:request/not-done not-done
                                  :request/version-nonnegative invariant-version})
              (add-operation :request/complete
                             (operation :request/example :claimed :done)))
        r (check! c)]
    (is (= #{:request/complete}
           (:unsupported-operation-ids (row r :request/not-done))))
    (is (= #{:request/claim :request/unclaim}
           (:preserving-operation-ids (row r :request/not-done))))
    (is (= :some-preservation-open (:reason (row r :request/not-done))))
    (is (= :checked-conditional-induction
           (:status (row r :request/version-nonnegative))))))

(deftest operation-coverage-and-identity-rejections-are-typed
  (rejects! (assoc catalog :operations [(first (:operations catalog))])
            catalog-error :operation-id-coverage-mismatch)
  (rejects! (update catalog :operations conj (first (:operations catalog)))
            catalog-error :duplicate-operation-id)
  (rejects! (update catalog :operations conj
                    {:id :request/extra :construction
                     (-> claim
                         (assoc-in [:subject :id] :request/extra)
                         (assoc-in [:subject :interface :operation] :request/extra))})
            catalog-error :operation-id-coverage-mismatch)
  (rejects! (change-operation catalog :request/unclaim
                              #(assoc-in % [:subject :id] :request/claim))
            catalog-error :operation-subject-id-mismatch)
  (rejects! (change-operation catalog :request/claim
                              #(assoc-in % [:subject :kind] :rule))
            catalog-error :obligation-source-contract-changed))

(deftest changing-one-state-type-or-footprint-fails-closed
  (rejects! (change-operation catalog :request/unclaim
                              #(assoc-in % [:state-types :request/status :values]
                                         #{:open :claimed :done :cancelled}))
            catalog-error :state-type-projection-mismatch)
  (rejects! (change-operation catalog :request/unclaim
                              #(assoc % :frame #{:request/version}))
            construction-error :frame-footprint-mismatch)
  (rejects! (change-operation catalog :request/unclaim
                              #(assoc % :writes #{:request/status :request/version}))
            construction-error :write-footprint-mismatch)
  (rejects! (change-operation catalog :request/unclaim
                              #(assoc % :reads #{}))
            :gesso.assurance.proof.transition/invalid-transition
            :read-footprint-mismatch))

(deftest operation-and-invariant-order-is-not-sequential-composition
  (let [r (check! catalog)
        reversed (check! (assoc catalog :operations
                                (vec (reverse (:operations catalog)))))
        reversed-invariants
        (check! (assoc catalog :invariants
                       (into (array-map) (reverse (seq base-invariants)))))]
    (is (= r reversed))
    (is (= r reversed-invariants))
    (is (= (vec (sort-by str (:declared-operation-ids catalog)))
           (mapv :operation-id (:operation-evidence r))))))

(deftest source-edits-force-fresh-identity
  (let [baseline (check! catalog)
        changes [(change-operation catalog :request/claim
                                   #(assoc-in % [:guard :expression]
                                              [:= [:read :request/status]
                                               [:literal :claimed]]))
                 (change-operation catalog :request/claim
                                   #(assoc-in % [:subject :interface :revision] 2))
                 (change-operation catalog :request/unclaim
                                   #(assoc-in % [:replacements :request/status]
                                              [:literal :claimed]))
                 (assoc catalog :initial-condition
                        {:readers #{:request/status}
                         :expression (:expression invariant-domain)})
                 (update catalog :invariants dissoc :request/truth)]]
    (doseq [change changes]
      (let [r (check! change)]
        (is (not= (:catalog-fingerprint baseline) (:catalog-fingerprint r)))
        (is (not= (:fingerprint baseline) (:fingerprint r)))
        (is (= :not-issued (:certificate r)))))))

(deftest narrowing-declared-catalog-does-not-claim-physical-completeness
  (let [smaller (-> catalog
                    (assoc :declared-operation-ids #{:request/claim})
                    (assoc :operations [{:id :request/claim :construction claim}]))
        r (check! smaller)]
    (is (:all-declared-invariants-conditionally-inductive? r))
    (is (= #{:request/claim} (:declared-operation-ids r)))
    (is (= :not-established (:external-operation-set-completeness r)))
    (is (= :not-established (:physical-write-path-coverage r)))
    (is (= :not-established (:implementation-correspondence r)))
    (is (= :none (:authority r)))
    (is (not= (:fingerprint r) (:fingerprint (check! catalog))))))

(deftest forged-sibling-reports-cannot-promote-the-outcome
  (let [real initial/check-initial-consequence!
        id [:initial-state :request/domain]]
    (with-redefs [initial/check-initial-consequence!
                  (fn [source]
                    (let [r (real source)]
                      (-> r
                          (update :initial-outcomes
                                  (fn [outcomes]
                                    (mapv (fn [o]
                                            (if (= id (:obligation-id o))
                                              (assoc o :status :open :rule nil
                                                     :reason :unsupported-initial-implication)
                                              o)) outcomes)))
                          (update :conditionally-checked-initial-ids disj id)
                          (update :unsupported-initial-ids conj id))))]
      (let [e (err #(check! catalog))]
        (is (= induction-error (:error/type e)))
        (is (= :sibling-checker-contract-changed (:reason e)))
        (is (= :initial (:checker e))))))
  (let [real preservation/check-unified-preservation!
        id [:preservation :request/version-nonnegative]
        altered (assoc catalog :invariants
                       (assoc base-invariants
                              :request/unsupportable
                              {:readers #{:request/version}
                               :expression [:< [:read :request/version]
                                            [:literal 0]]}))]
    (with-redefs [preservation/check-unified-preservation!
                  (fn [source]
                    (let [r (real source)]
                      (-> r
                          (update :preservation-outcomes
                                  (fn [xs]
                                    (mapv (fn [o]
                                            (if (= [:preservation :request/unsupportable]
                                                   (:obligation-id o))
                                              (assoc o :status :checked-conditional-inference
                                                     :rule :forged-rule)
                                              o)) xs)))
                          (update :conditionally-checked-preservation-ids
                                  conj [:preservation :request/unsupportable])
                          (update :unsupported-preservation-ids
                                  disj [:preservation :request/unsupportable]))))]
      (let [e (err #(check! altered))]
        (is (= induction-error (:error/type e)))
        (is (= :sibling-checker-contract-changed (:reason e)))
        (is (= :unified-preservation (:checker e))))))
  (let [real operation-set/check-operation-set!]
    (with-redefs [operation-set/check-operation-set!
                  (fn [c]
                    (-> (real c)
                        (assoc :certificate :issued :admission :admitted
                               :authority :authoritative)))]
      (let [e (err #(check! catalog))]
        (is (= induction-error (:error/type e)))
        (is (= :sibling-checker-contract-changed (:reason e)))
        (is (= :operation-set (:checker e)))))))

(deftest forged-obligation-ledger-rejected-at-earliest-source-boundary
  (let [real obligation/derive-obligations!]
    (with-redefs [obligation/derive-obligations!
                  (fn [source]
                    (-> (real source)
                        (assoc :proof-status :proved
                               :certificate :issued :authority :all)))]
      (let [e (err #(check! catalog))]
        (is (= catalog-error (:error/type e)))
        (is (= :obligation-checker-contract-changed (:reason e)))))))

(deftest copyable-reports-and-forged-validity-flags-are-not-inputs
  (let [r (check! catalog)
        forged (assoc r :valid? true :fully-certified? true
                      :authority :global :certificate :issued)
        c (assoc catalog :proof forged)]
    (is (= :none (:authority r)))
    (is (= :not-issued (:certificate r)))
    (is (sha256? (:fingerprint forged)))
    (rejects! c catalog-error :wrong-keys)
    (rejects! forged catalog-error :wrong-keys)
    (is (= r (check! catalog)))))

(deftest optional-absence-and-null-do-not-become-implicit-proof
  (let [c (assoc catalog
                 :invariants (assoc base-invariants
                                    :request/owner-present
                                    {:readers #{:request/owner}
                                     :expression [:present [:read :request/owner]]}
                                    :request/owner-not-null
                                    {:readers #{:request/owner}
                                     :expression [:not= [:read :request/owner]
                                                      [:literal nil]]}))
        r (check! c)]
    (doseq [id [:request/owner-present :request/owner-not-null]]
      (is (= :open (:status (row r id))))
      (is (= :initial-consequence-open (:reason (row r id)))))
    (is (= :not-established (:initial-state-existence r)))
    (is (= :not-established (:external-initialization-coverage r)))))

(deftest independently-enumerated-finite-traces-are-only-test-witnesses
  ;; This independently constructed tiny model checks the exact example's
  ;; status-transition shape, over explicit bounded values. It is NOT a
  ;; quantification over all Clojure, integers, XTDB effects or application
  ;; initialization. The candidate itself must remain unadmitted.
  (let [statuses [:open :claimed :done]
        versions [-1 0 1 2]
        actions [[:request/claim :open :claimed]
                 [:request/unclaim :claimed :open]]
        inv? (fn [{:keys [status version]}]
               (and (contains? #{:open :claimed :done} status)
                    (<= 0 version)))
        initial? (fn [{:keys [status version]}]
                   (and (= status :open) (<= 0 version)))
        states (vec (for [status statuses version versions]
                      {:status status :version version}))
        transitions (vec (for [state states
                               [id from to] actions
                               :when (= (:status state) from)]
                           {:operation id :pre state
                            :post (assoc state :status to)}))
        r (check! catalog)]
    (is (= 12 (count states)))
    (is (= 8 (count transitions)))
    (is (some initial? states))
    (is (every? (fn [s] (or (not (initial? s)) (inv? s))) states))
    (is (every? (fn [{:keys [pre post]}]
                  (or (not (inv? pre)) (inv? post)))
                transitions))
    (is (:all-declared-invariants-conditionally-inductive? r))
    (is (= :not-independently-proved (:checker-soundness r)))
    (is (= :not-established (:implementation-correspondence r)))))

(deftest finite-counterexample-to-unsafe-version-copy-remains-open
  (let [copy-version (-> (operation :request/unclaim :claimed :open)
                         (assoc :input-types {:input/version {:type :integer}}
                                :reads #{:request/status :input/version}
                                :writes #{:request/status :request/version}
                                :replacements {:request/status [:literal :open]
                                               :request/version [:read :input/version]}
                                :frame #{:request/owner}))
        changed (change-operation catalog :request/unclaim (constantly copy-version))
        r (check! changed)
        pre {:status :claimed :version 1}
        post {:status :open :version -1}]
    (is (<= 0 (:version pre)))
    (is (not (<= 0 (:version post))))
    (is (= :open (:status (row r :request/version-nonnegative))))
    (is (= #{:request/unclaim}
           (:unsupported-operation-ids (row r :request/version-nonnegative))))
    (is (= :none (:authority r)))))

(deftest oversized-and-empty-catalogs-reject-before-inference
  (rejects! (assoc catalog :declared-operation-ids #{})
            catalog-error :invalid-declared-operation-ids)
  (let [entries (vec (for [i (range 33)]
                       (keyword "request" (str "op-" i))))]
    (rejects! (assoc catalog :declared-operation-ids (set entries))
              catalog-error :invalid-declared-operation-ids)
    (rejects! (assoc catalog :operations
                     (vec (repeat 33 (first (:operations catalog)))))
              catalog-error :invalid-operation-sequence)))

(deftest no-source-exposure-or-obligation-erasure-after-repeated-checks
  (let [r1 (check! catalog)
        r2 (check! catalog)]
    (is (= r1 r2))
    (is (= (set (keys base-invariants))
           (set (map :invariant (:induction-outcomes r1)))))
    (is (= (get r1 :original-open-obligation-ids-by-operation)
           (get r2 :original-open-obligation-ids-by-operation)))
    (is (= :not-issued (:certificate r2)))
    (is (= :not-admitted (:admission r2)))
    (is (= :none (:authority r2)))))

(ns gesso.assurance.proof.operation-set-test
  "v1277 permanent adversarial coverage for the v1276 closed DECLARED
   abstract operation-set checker. A source-owned catalog is not an
   enumeration of physical XTDB writers, an inductive proof, certificate,
   runtime authorization, or deployment admission. Every original operation
   and shared model rule must still be rechecked independently."
  (:require [clojure.set :as set]
            [clojure.test :refer [deftest is testing]]
            [gesso.assurance.proof.obligation :as obligation]
            [gesso.assurance.proof.operation-set :as operation-set]
            [gesso.assurance.proof.transition :as abstract-transition]
            [gesso.assurance.transition :as construction]))

(def ^:private state-types
  {:request/status {:type :enum :name :request/status
                    :values #{:open :claimed :done}}
   :request/owner {:type :keyword :nullable? true}
   :request/version {:type :integer}
   :request/note {:type :string :nullable? true :optional? true}})

(defn- operation [id guard-status writes replacements input-types reads]
  {:subject {:kind :operation :id id :interface {:operation id}}
   :state-types state-types
   :input-types input-types
   :guard {:readers #{:request/status}
           :expression [:= [:read :request/status] [:literal guard-status]]}
   :reads reads
   :writes writes
   :replacements replacements
   :frame (set/difference (set (keys state-types)) writes)})

(def ^:private claim
  (operation :request/claim :open
             #{:request/status :request/owner}
             {:request/status [:literal :claimed]
              :request/owner [:read :input/actor]}
             {:input/actor {:type :keyword}}
             #{:request/status :input/actor}))

(def ^:private unclaim
  (operation :request/unclaim :claimed
             #{:request/status :request/owner}
             {:request/status [:literal :open]
              :request/owner [:literal nil]}
             {} #{:request/status}))

(def ^:private complete
  (operation :request/complete :claimed
             #{:request/status}
             {:request/status [:literal :done]}
             {} #{:request/status}))

(def ^:private initial-condition
  {:readers #{:request/status :request/version}
   :expression [:and
                [:= [:read :request/status] [:literal :open]]
                [:>= [:read :request/version] [:literal 0]]]})

(def ^:private invariants
  {:request/status-domain
   {:readers #{:request/status}
    :expression [:in [:read :request/status]
                 [:literal #{:open :claimed :done}]]}
   :request/version-nonnegative
   {:readers #{:request/version}
    :expression [:>= [:read :request/version] [:literal 0]]}
   :request/optional-note-present
   {:readers #{:request/note}
    :expression [:present [:read :request/note]]}
   :request/truth
   {:readers #{} :expression [:literal true]}})

(def ^:private catalog
  {:id :request/operation-catalog
   :declared-operation-ids #{:request/claim :request/unclaim :request/complete}
   :initial-condition initial-condition
   :invariants invariants
   :operations [{:id :request/claim :construction claim}
                {:id :request/unclaim :construction unclaim}
                {:id :request/complete :construction complete}]})

(def ^:private catalog-error
  :gesso.assurance.proof.operation-set/invalid-operation-set)
(def ^:private obligation-error
  :gesso.assurance.proof.obligation/invalid-obligation-source)
(def ^:private construction-error
  :gesso.assurance.transition/invalid-construction)
(def ^:private transition-error
  :gesso.assurance.proof.transition/invalid-transition)

(defn- check! [c]
  (operation-set/check-operation-set! c))

(defn- err [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn- reject!
  [c error-type reason]
  (let [e (err #(check! c))]
    (is (map? e) (str "Expected typed rejection " reason))
    (is (= error-type (:error/type e)) (pr-str e))
    (is (= reason (:reason e)) (pr-str e))
    (is (vector? (:at e)) (pr-str e))
    e))

(defn- catalog-reject! [c reason]
  (reject! c catalog-error reason))

(defn- alter-operation [c id f]
  (update c :operations
          (fn [xs] (mapv #(if (= id (:id %)) (f %) %) xs))))

(defn- alter-construction [c id f]
  (alter-operation c id #(update % :construction f)))

(defn- entry-source [c id]
  {:construction (:construction (first (filter #(= id (:id %))
                                             (:operations c))))
   :initial-condition (:initial-condition c)
   :invariants (:invariants c)})

(defn- result [r id]
  (first (filter #(= id (:operation-id %)) (:operation-results r))))

(defn- sha256? [x]
  (and (string? x) (boolean (re-matches #"sha256:[0-9a-f]{64}" x))))

(defn- renamed [old-operation id]
  (-> old-operation
      (assoc-in [:subject :id] id)
      (assoc-in [:subject :interface :operation] id)))

(defn- add-operation [c id original]
  (-> c
      (update :declared-operation-ids conj id)
      (update :operations conj {:id id
                                :construction (renamed original id)})))

(deftest catalog-has-exact-source-owned-report-contract
  (let [r (check! catalog)]
    (is (= operation-set/report-type (:type r)))
    (is (= operation-set/operation-set-version (:version r)))
    (is (sha256? (:fingerprint r)))
    (is (= (:id catalog) (:catalog-id r)))
    (is (= (:declared-operation-ids catalog)
           (:declared-operation-ids r)))
    (is (= :checked (:exact-declared-operation-coverage r)))
    (is (= (set (keys state-types)) (:state-projection r)))
    (is (= (set (keys invariants)) (:invariant-ids r)))
    (is (= 3 (count (:operation-results r))))
    (is (= [:request/claim :request/complete :request/unclaim]
           (mapv :operation-id (:operation-results r))))
    (is (= (set (map :operation-id (:operation-results r)))
           (:declared-operation-ids r)))
    (is (= (:declared-operation-ids r)
           (set (keys (:open-obligation-ids-by-operation r)))))))

(deftest every-operation-references-its-own-original-obligation-ledger
  (let [r (check! catalog)]
    (doseq [id (:declared-operation-ids catalog)
            :let [ledger (obligation/derive-obligations!
                          (entry-source catalog id))
                  entry (result r id)]]
      (is (= id (:operation-id entry)))
      (is (= (:fingerprint ledger) (:obligation-fingerprint entry)))
      (is (= (:transition-fingerprint ledger)
             (:transition-fingerprint entry)))
      (is (= (:subject-fingerprint ledger) (:subject-fingerprint entry)))
      (is (= (:open-obligation-ids ledger)
             (:open-obligation-ids entry)))
      (is (= (:open-obligation-ids ledger)
             (get (:open-obligation-ids-by-operation r) id)))
      (is (every? sha256?
                  [(:obligation-fingerprint entry)
                   (:transition-fingerprint entry)
                   (:subject-fingerprint entry)])))))

(deftest each-operation-has-every-declared-preservation-and-initial-obligation
  (let [r (check! catalog)
        initial-ids (set (map #(vector :initial-state %)
                              (keys invariants)))
        preservation-ids (set (map #(vector :preservation %)
                                   (keys invariants)))]
    (doseq [entry (:operation-results r)
            :let [ids (:open-obligation-ids entry)]]
      (is (set/subset? initial-ids ids))
      (is (set/subset? preservation-ids ids))
      (is (contains? ids [:assumption :initial-state-existence]))
      (is (contains? ids [:assumption :initial-state-coverage]))
      (is (contains? ids [:assumption :declared-state-projection-coverage]))
      (is (contains? ids [:assumption :declared-invariant-set-coverage]))
      (is (contains? ids [:assumption :checker-meta-soundness]))
      (is (contains? ids [:implementation :atomic-transaction-enforcement]))
      (is (contains? ids [:implementation :current-runtime-authorization]))
      (is (contains? ids [:implementation :physical-effect-containment])))))

(deftest distinct-operations-retain-distinct-sources-and-footprints
  (let [r (check! catalog)
        c (result r :request/claim)
        u (result r :request/unclaim)
        d (result r :request/complete)]
    (is (not= (:obligation-fingerprint c) (:obligation-fingerprint u)))
    (is (not= (:transition-fingerprint c) (:transition-fingerprint d)))
    (is (not= (:subject-fingerprint c) (:subject-fingerprint d)))
    (is (= (:reads claim) (:reads c)))
    (is (= (:writes claim) (:writes c)))
    (is (= (:frame claim) (:frame c)))
    (is (= (:reads unclaim) (:reads u)))
    (is (= (:writes unclaim) (:writes u)))
    (is (= (:frame unclaim) (:frame u)))
    (is (= (:frame complete) (:frame d)))
    (is (= #{:request/version :request/note :request/owner}
           (:frame d)))))

(deftest operation-order-is-semantically-irrelevant
  (let [expected (check! catalog)
        ops (:operations catalog)]
    (doseq [xs [(vec (reverse ops))
                (conj (vec (rest ops)) (first ops))]]
      (is (= expected (check! (assoc catalog :operations xs)))))))

(deftest all-permutations-of-the-declared-three-operation-set-agree
  (let [ops (:operations catalog)
        expected (check! catalog)]
    (doseq [a ops b ops c ops :when (= 3 (count (set [(:id a) (:id b) (:id c)])))]
      (is (= expected (check! (assoc catalog :operations [a b c])))))))

(deftest rechecking-the-same-source-is-deterministic
  (is (= (check! catalog) (check! catalog))))

(deftest inserting-an-operation-changes-catalog-identity
  (let [extended (add-operation catalog :request/reopen unclaim)]
    (is (not= (:fingerprint (check! catalog))
              (:fingerprint (check! extended))))
    (is (= 4 (count (:operation-results (check! extended)))))))

(deftest deleting-an-operation-requires-updating-the-declared-set
  (let [bad (update catalog :operations
                    (fn [xs] (vec (remove #(= :request/complete (:id %)) xs))))]
    (catalog-reject! bad :operation-id-coverage-mismatch)
    (let [closed (update bad :declared-operation-ids disj :request/complete)]
      (is (= #{:request/claim :request/unclaim}
             (:declared-operation-ids (check! closed))))
      (is (not= (:fingerprint (check! catalog))
                (:fingerprint (check! closed)))))))

(deftest undeclared-addition-rejected-even-if-individually-valid
  (catalog-reject! (update catalog :operations conj
                           {:id :request/reopen
                            :construction (renamed unclaim :request/reopen)})
                   :operation-id-coverage-mismatch))

(deftest declared-addition-without-a-construction-rejected
  (catalog-reject! (update catalog :declared-operation-ids conj :request/reopen)
                   :operation-id-coverage-mismatch))

(deftest duplicate-operation-ids-rejected-before-obligation-derivation
  (catalog-reject! (update catalog :operations conj
                           (first (:operations catalog)))
                   :duplicate-operation-id))

(deftest duplicate-ids-cannot-be-concealed-by-changing-constructors
  (catalog-reject! (assoc-in catalog [:operations 2] (first (:operations catalog)))
                   :duplicate-operation-id))

(deftest id-mismatch-cannot-alias-another-operation
  (catalog-reject! (assoc-in catalog [:operations 0 :id] :request/unclaim)
                   :operation-subject-id-mismatch))

(deftest subject-id-mismatch-rejected-before-contract-checking
  (catalog-reject!
   (alter-construction catalog :request/unclaim
                       #(assoc-in % [:subject :id] :request/claim))
   :operation-subject-id-mismatch))

(deftest subject-kind-must-actually-be-an-operation
  (catalog-reject!
   (alter-construction catalog :request/unclaim
                       #(assoc-in % [:subject :kind] :rule))
   :obligation-source-contract-changed))

(deftest catalog-id-must-be-qualified
  (doseq [id [nil :catalog "request/catalog" 17]]
    (catalog-reject! (assoc catalog :id id) :invalid-catalog-id)))

(deftest operation-ids-must-be-qualified
  (doseq [id [nil :claim "request/claim" 99]]
    (catalog-reject! (assoc-in catalog [:operations 0 :id] id)
                     :invalid-operation-id)))

(deftest declared-set-requires-qualified-ids
  (doseq [id [:claim 42 "request/claim" nil]]
    (catalog-reject! (update catalog :declared-operation-ids conj id)
                     :invalid-declared-operation-ids)))

(deftest an-empty-declared-operation-set-is-not-a-closed-proof
  (catalog-reject! (assoc catalog :declared-operation-ids #{})
                   :invalid-declared-operation-ids))

(deftest an-empty-operation-sequence-is-not-a-closed-proof
  (catalog-reject! (assoc catalog :operations [])
                   :invalid-operation-sequence))

(deftest operation-set-size-limit-is-enforced-before-expensive-checking
  (let [ids (mapv #(keyword "request" (str "bulk-" %)) (range 33))
        entries (mapv (fn [id] {:id id :construction (renamed claim id)}) ids)]
    (catalog-reject! (assoc catalog :declared-operation-ids (set ids)
                            :operations entries)
                     :invalid-declared-operation-ids)
    (catalog-reject! (assoc catalog :declared-operation-ids #{:request/claim}
                            :operations entries)
                     :invalid-operation-sequence)))

(deftest catalog-membership-must-be-a-plain-set
  (doseq [x [[:request/claim :request/unclaim :request/complete]
             (list :request/claim :request/unclaim :request/complete)
             (sorted-set :request/claim :request/unclaim :request/complete)]]
    (catalog-reject! (assoc catalog :declared-operation-ids x)
                     :invalid-declared-operation-ids)))

(deftest catalog-operations-must-be-a-vector
  (doseq [x [(list (first (:operations catalog)))
             (seq (:operations catalog))
             #{(first (:operations catalog))}]]
    (catalog-reject! (assoc catalog :operations x)
                     :invalid-operation-sequence)))

(deftest catalog-source-must-be-a-plain-map
  (reject! [] catalog-error :expected-plain-map)
  (reject! nil catalog-error :expected-plain-map))

(deftest missing-and-extra-top-level-fields-rejected
  (doseq [key (keys catalog)]
    (catalog-reject! (dissoc catalog key) :wrong-keys))
  (doseq [key [:proved? :certificate :authority :version :state-types]]
    (catalog-reject! (assoc catalog key :granted) :wrong-keys)))

(deftest exact-operation-entry-shape-is-required
  (doseq [key [:id :construction]]
    (catalog-reject!
     (update catalog :operations #(assoc % 0 (dissoc (first %) key)))
     :wrong-keys))
  (doseq [key [:certificate :proof :authority :already-checked]]
    (catalog-reject! (assoc-in catalog [:operations 0 key] :accepted)
                     :wrong-keys)))

(deftest construction-entry-must-be-a-plain-map
  (doseq [value [nil [] :request/claim #{}]]
    (catalog-reject! (assoc-in catalog [:operations 0 :construction] value)
                     :expected-construction-source)))

(deftest metadata-on-catalog-and-its-sequences-is-rejected
  (catalog-reject! (with-meta catalog {:proof :trusted})
                   :metadata-not-allowed)
  (catalog-reject! (update catalog :operations
                           #(with-meta % {:complete true}))
                   :metadata-not-allowed)
  (catalog-reject! (update catalog :declared-operation-ids
                           #(with-meta % {:closed true}))
                   :metadata-not-allowed))

(deftest metadata-on-an-individual-operation-entry-is-rejected
  (catalog-reject!
   (update catalog :operations #(assoc % 0 (with-meta (first %) {:auth true})))
   :metadata-not-allowed))

(deftest metadata-on-original-construction-is-rejected
  (reject! (alter-construction catalog :request/claim
                                #(with-meta % {:auth true}))
           construction-error :metadata-not-allowed))

(deftest ordinary-guard-change-invalidates-catalog-fingerprint
  (let [modified (alter-construction
                  catalog :request/unclaim
                  #(assoc-in % [:guard :expression]
                             [:= [:read :request/status] [:literal :open]]))]
    (is (not= (:fingerprint (check! catalog))
              (:fingerprint (check! modified))))))

(deftest literal-assignment-change-invalidates-catalog-fingerprint
  (let [modified (alter-construction
                  catalog :request/unclaim
                  #(assoc-in % [:replacements :request/status]
                             [:literal :done]))]
    (is (not= (:fingerprint (check! catalog))
              (:fingerprint (check! modified))))))

(deftest subject-interface-change-invalidates-catalog-fingerprint
  (let [modified (alter-construction
                  catalog :request/complete
                  #(assoc-in % [:subject :interface]
                             {:operation :request/complete :version 2}))]
    (is (not= (:fingerprint (check! catalog))
              (:fingerprint (check! modified))))))

(deftest shared-initial-rule-changes-catalog-fingerprint
  (let [modified (assoc-in catalog [:initial-condition :expression 2]
                           [:>= [:read :request/version] [:literal 1]])]
    (is (not= (:fingerprint (check! catalog))
              (:fingerprint (check! modified))))))

(deftest shared-invariant-changes-catalog-fingerprint
  (let [modified (assoc-in catalog
                           [:invariants :request/status-domain :expression 2]
                           [:literal #{:open :claimed}])]
    (is (not= (:fingerprint (check! catalog))
              (:fingerprint (check! modified))))))

(deftest added-and-removed-invariants-change-catalog-fingerprint
  (let [base (:fingerprint (check! catalog))
        added (assoc-in catalog [:invariants :request/false]
                        {:readers #{} :expression [:literal false]})
        removed (update catalog :invariants dissoc :request/truth)]
    (is (not= base (:fingerprint (check! added))))
    (is (not= base (:fingerprint (check! removed))))))

(deftest catalog-name-change-only-changes-catalog-semantic-identity
  (let [a (check! catalog)
        b (check! (assoc catalog :id :request/other-catalog))]
    (is (not= (:fingerprint a) (:fingerprint b)))
    (is (= (:operation-results a) (:operation-results b)))
    (is (= (:invariant-ids a) (:invariant-ids b)))))

(deftest operation-input-type-change-affects-semantic-identity
  (let [modified (alter-construction
                  catalog :request/claim
                  #(assoc-in % [:input-types :input/actor]
                             {:type :keyword :name :request/actor}))]
    ;; A nominal change can reject the construction; either rejection or a
    ;; changed source identity is mandatory. It must never silently reuse the
    ;; original catalog evidence.
    (let [original (:fingerprint (check! catalog))
          outcome (try (check! modified)
                       (catch clojure.lang.ExceptionInfo e e))]
      (if (instance? clojure.lang.ExceptionInfo outcome)
        (is (keyword? (:error/type (ex-data outcome))))
        (is (not= original (:fingerprint outcome)))))))

(deftest changed-write-and-frame-footprint-invalidates-evidence
  (let [changed (alter-construction
                 catalog :request/complete
                 (fn [c] (-> c
                             (update :writes conj :request/owner)
                             (assoc-in [:replacements :request/owner]
                                       [:literal nil])
                             (update :frame disj :request/owner))))]
    (is (not= (:fingerprint (check! catalog))
              (:fingerprint (check! changed))))
    (is (= #{:request/version :request/note}
           (:frame (result (check! changed) :request/complete))))))

(deftest invalid-frame-is-not-silently-repaired-by-the-catalog
  (reject! (alter-construction catalog :request/claim
                               #(assoc % :frame #{}))
           construction-error :frame-footprint-mismatch))

(deftest writes-must-equal-replacement-keys
  (reject! (alter-construction catalog :request/complete
                               #(update % :writes conj :request/owner))
           construction-error :write-footprint-mismatch))

(deftest phantom-read-footprints-are-not-accepted
  (reject! (alter-construction catalog :request/unclaim
                               #(assoc % :reads #{}))
           transition-error :read-footprint-mismatch))

(deftest state-projection-cannot-change-on-only-one-operation
  (let [modified (alter-construction
                  catalog :request/unclaim
                  #(assoc-in % [:state-types :request/status :values]
                             #{:open :claimed :done :cancelled}))]
    (catalog-reject! modified :state-type-projection-mismatch)))

(deftest optionality-change-in-one-operation-cannot-form-a-shared-model
  (let [modified (alter-construction
                  catalog :request/complete
                  #(assoc-in % [:state-types :request/note :optional?] false))]
    (catalog-reject! modified :state-type-projection-mismatch)))

(deftest invalid-nullable-integer-invariant-is-rejected-before-model-comparison
  (let [modified (alter-construction
                  catalog :request/complete
                  #(assoc-in % [:state-types :request/version]
                             {:type :integer :nullable? true}))]
    (reject! modified :gesso.assurance.rule-type/invalid-rule-type
             :expected-nonnullable-integers)))

(deftest all-operation-state-types-may-change-together-with-fresh-evidence
  (let [modified (update catalog :operations
                         #(mapv (fn [entry]
                                  (update-in entry [:construction :state-types]
                                             assoc :request/extra {:type :boolean
                                                                   :optional? true}))
                                (mapv (fn [entry]
                                        (update-in entry [:construction :frame]
                                                   conj :request/extra)) %)))]
    (let [r (check! modified)]
      (is (= (conj (set (keys state-types)) :request/extra)
             (:state-projection r)))
      (is (not= (:fingerprint r) (:fingerprint (check! catalog)))))))

(deftest different-guards-do-not-imply-operation-sequence-or-reachability
  (let [r (check! catalog)]
    (is (= :closed-declared-abstract-operation-set-only (:scope r)))
    (is (= :declared-abstract-operation-set-checked (:proof-status r)))
    (is (nil? (:reachable-states r)))
    (is (nil? (:operation-order r)))
    (is (nil? (:inductive-theorem r)))))

(deftest unchanged-operation-ledgers-remain-stable-under-sibling-source-changes
  (let [a (check! catalog)
        b (check! (alter-construction catalog :request/unclaim
                                     #(assoc-in % [:guard :expression]
                                                [:= [:read :request/status]
                                                 [:literal :open]])))]
    (is (not= (:fingerprint a) (:fingerprint b)))
    (is (= (result a :request/claim) (result b :request/claim)))
    (is (= (result a :request/complete) (result b :request/complete)))
    (is (not= (result a :request/unclaim) (result b :request/unclaim)))))

(deftest modifying-a-shared-invariant-updates-all-operation-ledgers
  (let [changed (assoc-in catalog [:invariants :request/truth :expression]
                          [:literal false])
        a (check! catalog)
        b (check! changed)]
    (doseq [id (:declared-operation-ids catalog)]
      (is (not= (:obligation-fingerprint (result a id))
                (:obligation-fingerprint (result b id)))))))

(deftest function-execution-is-not-accepted-as-a-guard-expression
  (let [bad (alter-construction catalog :request/claim
                                #(assoc-in % [:guard :expression]
                                           (fn [_] true)))
        e (err #(check! bad))]
    (is (map? e))
    (is (keyword? (:error/type e)))
    (is (not= catalog-error (:error/type e)))))

(deftest forged-top-level-certificate-or-authority-is-not-valid-source
  (doseq [[k v] [[:certificate :issued]
                 [:admission :admitted]
                 [:authority :transaction]
                 [:proof-status :proved]
                 [:external-operation-set-completeness :established]]]
    (catalog-reject! (assoc catalog k v) :wrong-keys)))

(deftest reusing-the-report-as-a-catalog-is-not-accepted
  (catalog-reject! (check! catalog) :wrong-keys))

(deftest forged-per-operation-obligation-ledger-is-rejected
  (let [real obligation/derive-obligations!
        source (atom nil)]
    (with-redefs [obligation/derive-obligations!
                  (fn [s]
                    (reset! source s)
                    (assoc (real s) :authority :granted))]
      (catalog-reject! catalog :obligation-checker-contract-changed))
    (is (map? @source))
    (is (= #{:construction :initial-condition :invariants}
           (set (keys @source))))))

(deftest internally-consistent-forged-obligation-success-is-rejected
  (let [real obligation/derive-obligations!]
    (with-redefs [obligation/derive-obligations!
                  (fn [s]
                    (let [r (real s)]
                      (-> r
                          (assoc :status :all-obligations-proved
                                 :proof-status :proved
                                 :certificate :issued
                                 :admission :admitted
                                 :authority :granted
                                 :open-obligation-ids #{}
                                 :obligations (into {}
                                                    (for [[k os] (:obligations r)]
                                                      [k (mapv #(assoc % :status :proved) os)]))))))]
      (catalog-reject! catalog :obligation-checker-contract-changed))))

(deftest forged-ledger-with-truncated-obligations-is-rejected
  (let [real obligation/derive-obligations!]
    (with-redefs [obligation/derive-obligations!
                  (fn [s]
                    (let [r (real s)]
                      (-> r
                          (update-in [:obligations :preservation] pop)
                          (update :open-obligation-ids disj
                                  [:preservation :request/truth]))))]
      (catalog-reject! catalog :obligation-checker-contract-changed))))

(deftest copying-an-earlier-real-ledger-cannot-impersonate-a-different-operation
  (let [real obligation/derive-obligations!
        original (real (entry-source catalog :request/claim))]
    (with-redefs [obligation/derive-obligations!
                  (fn [s] (if (= :request/unclaim
                                (get-in s [:construction :subject :id]))
                            original
                            (real s)))]
      (catalog-reject! catalog :obligation-checker-contract-changed))))

(deftest source-provided-obligation-report-is-not-accepted-as-evidence
  (reject!
   (assoc-in catalog [:operations 0 :construction :proof]
             (obligation/derive-obligations!
              (entry-source catalog :request/claim)))
   construction-error :wrong-keys))

(deftest an-undeclared-real-world-writer-remains-an-open-assumption
  (let [r (check! catalog)]
    (is (= :not-established (:external-operation-set-completeness r)))
    (is (= :not-established (:physical-write-path-coverage r)))
    (is (contains? (:open-meta-obligations r)
                   :external-operation-set-completeness))
    (is (contains? (:open-meta-obligations r)
                   :physical-write-path-coverage))))

(deftest initial-state-and-invariant-coverage-remain-open
  (let [r (check! catalog)]
    (is (= :not-established (:initialization-coverage r)))
    (is (contains? (:open-meta-obligations r) :initialization-coverage))
    (is (contains? (:open-meta-obligations r) :abstract-invariant-preservation))
    (is (contains? (:open-meta-obligations r) :checker-meta-soundness))
    (is (contains? (:open-meta-obligations r) :certificate-admission))))

(deftest no-universal-preservation-certificate-or-commit-authority-is-issued
  (let [r (check! catalog)]
    (is (= :not-established (:invariant-preservation r)))
    (is (= :not-issued (:certificate r)))
    (is (= :not-admitted (:admission r)))
    (is (= :not-established (:runtime-enforcement r)))
    (is (= :not-established (:implementation-correspondence r)))
    (is (= :not-independently-proved (:checker-soundness r)))
    (is (= :none (:authority r)))
    (is (= :original-typed-operation-sources-and-shared-rules
           (:checking r)))))

(deftest guarded-constructions-are-still-checked-by-their-independent-source
  (doseq [{:keys [id construction]} (:operations catalog)]
    (let [description (construction/build-guarded-replacement! construction)
          semantics (abstract-transition/describe-transition!
                     (:transition description))
          entry (result (check! catalog) id)]
      (is (= (:reads entry) (get-in semantics [:semantics :reads])))
      (is (= (:writes entry) (get-in semantics [:semantics :writes])))
      (is (= (:frame entry) (:frame-fields (:semantics semantics)))))))

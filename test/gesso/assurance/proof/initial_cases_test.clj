(ns gesso.assurance.proof.initial-cases-test
  "v1275 permanent adversarial tests for the finite literal initial-state
   constructor catalog (v1274). Only the declared finite case set is covered.
   This is NOT Init => I for arbitrary states, an independently proved rule,
   coverage of external/XTDB initializers, proof admission, or authority."
  (:require [clojure.set :as set]
            [clojure.test :refer [deftest is testing]]
            [gesso.assurance.proof.initial-cases :as cases]
            [gesso.assurance.proof.initial-constructor :as constructor]
            [gesso.assurance.proof.obligation :as obligation]
            [gesso.assurance.rule-eval :as rule-eval]))

(def ^:private claim
  {:subject {:kind :operation :id :example/claim
             :interface {:operation :example/claim}}
   :state-types
   {:request/status {:type :enum :name :request/status
                     :values #{:open :claimed :done}}
    :request/owner {:type :keyword :nullable? true}
    :request/version {:type :integer}
    :request/note {:type :string :optional? true :nullable? true}
    :request/flag {:type :boolean :optional? true}}
   :input-types {:input/actor {:type :keyword}}
   :guard {:readers #{:request/status}
           :expression [:= [:read :request/status] [:literal :open]]}
   :reads #{:request/status :input/actor}
   :writes #{:request/status :request/owner}
   :replacements {:request/status [:literal :claimed]
                  :request/owner [:read :input/actor]}
   :frame #{:request/version :request/note :request/flag}})

(def ^:private initial-condition
  {:readers #{:request/status :request/owner :request/version}
   :expression [:and
                [:= [:read :request/status] [:literal :open]]
                [:= [:read :request/owner] [:literal nil]]
                [:>= [:read :request/version] [:literal 0]]]})

(def ^:private invariants
  {:request/status-allowed
   {:readers #{:request/status}
    :expression [:in [:read :request/status]
                 [:literal #{:open :claimed :done}]]}
   :request/owner-empty
   {:readers #{:request/owner}
    :expression [:= [:read :request/owner] [:literal nil]]}
   :request/version-nonnegative
   {:readers #{:request/version}
    :expression [:>= [:read :request/version] [:literal 0]]}
   :request/version-at-most-three
   {:readers #{:request/version}
    :expression [:<= [:read :request/version] [:literal 3]]}
   :request/note-absent
   {:readers #{:request/note}
    :expression [:not [:present [:read :request/note]]]}
   :request/true
   {:readers #{} :expression [:literal true]}})

(def ^:private source
  {:construction claim
   :initial-condition initial-condition
   :invariants invariants})

(def ^:private create-case
  {:id :request/new
   :fields {:request/status [:literal :open]
            :request/owner [:literal nil]
            :request/version [:literal 0]
            :request/note [:absent]
            :request/flag [:absent]}})

(def ^:private import-case
  {:id :request/import
   :fields {:request/status [:literal :open]
            :request/owner [:literal nil]
            :request/version [:literal 1]
            :request/note [:absent]
            :request/flag [:absent]}})

(def ^:private catalog
  {:id :request/initial-catalog
   :declared-constructor-ids #{:request/new :request/import}
   :constructors [create-case import-case]})

(def ^:private catalog-error
  :gesso.assurance.proof.initial-cases/invalid-initial-cases)
(def ^:private constructor-error
  :gesso.assurance.proof.initial-constructor/invalid-initial-constructor)
(def ^:private evaluation-error
  :gesso.assurance.rule-eval/invalid-rule-evaluation)
(def ^:private obligation-error
  :gesso.assurance.proof.obligation/invalid-obligation-source)

(defn- check!
  ([c] (cases/check-initial-cases! source c))
  ([s c] (cases/check-initial-cases! s c)))

(defn- err [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn- rejects!
  ([c error-type reason] (rejects! source c error-type reason))
  ([s c error-type reason]
   (let [e (err #(check! s c))]
     (is (map? e) (str "Expected typed rejection " reason))
     (is (= error-type (:error/type e)) (pr-str e))
     (is (= reason (:reason e)) (pr-str e))
     (is (vector? (:at e)) (pr-str e))
     e)))

(defn- change-case [c id f]
  (update c :constructors
          (fn [xs] (mapv #(if (= (:id %) id) (f %) %) xs))))

(defn- field [c id k expression]
  (change-case c id #(assoc-in % [:fields k] expression)))

(defn- outcome [r invariant]
  (first (filter #(= invariant (:invariant %))
                 (:initial-catalog-outcomes r))))

(defn- case-result [r id]
  (first (filter #(= id (:constructor-id %)) (:case-results r))))

(defn- initial-ids [s]
  (set (for [id (keys (:invariants s))] [:initial-state id])))

(defn- all-open [s]
  (:open-obligation-ids (obligation/derive-obligations! s)))

(deftest exact-source-bound-catalog-report
  (let [r (check! catalog)
        l (obligation/derive-obligations! source)
        ids (initial-ids source)]
    (is (= cases/report-type (:type r)))
    (is (= cases/initial-cases-version (:version r)))
    (is (re-matches #"sha256:[0-9a-f]{64}" (:fingerprint r)))
    (is (= (:id catalog) (:catalog-id r)))
    (is (= (:declared-constructor-ids catalog)
           (:declared-constructor-ids r)))
    (is (= :exact (:declared-id-coverage r)))
    (is (= (:fingerprint l) (:obligation-fingerprint r)))
    (is (= (:transition-fingerprint l) (:transition-fingerprint r)))
    (is (= (:subject-fingerprint l) (:subject-fingerprint r)))
    (is (= 2 (count (:case-results r))))
    (is (= #{:request/new :request/import}
           (set (map :constructor-id (:case-results r)))))
    (is (= ids (set (map :initial-obligation-id
                        (:initial-catalog-outcomes r)))))
    (is (= (count ids) (count (:initial-catalog-outcomes r))))
    (is (= ids (:checked-declared-case-initial-ids r)))
    (is (= #{} (:violated-declared-case-initial-ids r)))
    (is (true? (:all-declared-cases-satisfy-invariants? r)))
    (is (= (:open-obligation-ids l) (:original-open-obligation-ids r)))))

(deftest each-constructor-is-independently-rechecked
  (let [r (check! catalog)]
    (doseq [case [create-case import-case]
            :let [single (constructor/check-initial-constructor! source case)
                  combined (case-result r (:id case))]]
      (is (= (:fingerprint single) (:constructor-fingerprint combined)))
      (is (= (:constructed-state single) (:constructed-state combined)))
      (is (= (:checked-initial-case-ids single)
             (:checked-initial-case-ids combined)))
      (is (= (:violated-initial-case-ids single)
             (:violated-initial-case-ids combined))))))

(deftest outcomes-have-exact-disjoint-coverage
  (let [r (check! catalog)
        checked (:checked-declared-case-initial-ids r)
        violated (:violated-declared-case-initial-ids r)]
    (is (= (initial-ids source) (set/union checked violated)))
    (is (empty? (set/intersection checked violated)))
    (doseq [id (keys invariants)]
      (is (= {:initial-obligation-id [:initial-state id]
              :invariant id
              :status :checked-across-declared-cases
              :violating-constructor-ids #{}}
             (outcome r id))))))

(deftest catalog-order-is-not-semantic-order
  (let [forward (check! catalog)
        reverse (check! (update catalog :constructors #(vec (reverse %))))]
    (is (= forward reverse))
    (is (= (:fingerprint forward) (:fingerprint reverse)))
    (is (= [:request/import :request/new]
           (mapv :constructor-id (:case-results forward))))))

(deftest rechecking-same-source-and-catalog-is-deterministic
  (let [a (check! catalog) b (check! catalog)]
    (is (= a b))
    (is (= (:fingerprint a) (:fingerprint b)))
    (is (= (:original-open-obligation-ids a)
           (:original-open-obligation-ids b)))))

(deftest catalog-id-is-part-of-evidence-identity
  (let [a (check! catalog)
        b (check! (assoc catalog :id :request/imported-catalog))]
    (is (not= (:fingerprint a) (:fingerprint b)))
    (is (= (:case-results a) (:case-results b)))
    (is (= (:obligation-fingerprint a) (:obligation-fingerprint b)))))

(deftest adding-declared-constructor-changes-identity
  (let [c (-> catalog
              (update :declared-constructor-ids conj :request/seed)
              (update :constructors conj
                      (assoc create-case :id :request/seed)))
        a (check! catalog) b (check! c)]
    (is (= 3 (count (:case-results b))))
    (is (not= (:fingerprint a) (:fingerprint b)))
    (is (true? (:all-declared-cases-satisfy-invariants? b)))))

(deftest replacing-a-case-value-changes-evidence-identity
  (let [a (check! catalog)
        b (check! (field catalog :request/import :request/version [:literal 2]))]
    (is (not= (:fingerprint a) (:fingerprint b)))
    (is (= (:obligation-fingerprint a) (:obligation-fingerprint b)))
    (is (= 2 (get-in (case-result b :request/import)
                     [:constructed-state :request/version])))))

(deftest identical-literal-numeric-representations-have-same-identity
  (let [a (check! catalog)
        b (check! (field catalog :request/import :request/version [:literal 1N]))]
    (is (= (:fingerprint a) (:fingerprint b)))
    (is (= (:case-results a) (:case-results b)))))

(deftest a-violated-invariant-stays-visible-across-the-catalog
  (let [c (field catalog :request/import :request/version [:literal 4])
        r (check! c)
        id [:initial-state :request/version-at-most-three]]
    (is (false? (:all-declared-cases-satisfy-invariants? r)))
    (is (= #{id} (:violated-declared-case-initial-ids r)))
    (is (= (disj (initial-ids source) id)
           (:checked-declared-case-initial-ids r)))
    (is (= :violated-declared-case
           (:status (outcome r :request/version-at-most-three))))
    (is (= #{:request/import}
           (:violating-constructor-ids
            (outcome r :request/version-at-most-three))))
    (is (contains? (:violated-initial-case-ids (case-result r :request/import))
                   id))
    (is (not (contains? (:violated-initial-case-ids
                         (case-result r :request/new)) id)))))

(deftest multiple-violating-constructors-are-all-listed
  (let [c (-> catalog
              (field :request/new :request/version [:literal 7])
              (field :request/import :request/version [:literal 4]))
        r (check! c)]
    (is (= #{:request/new :request/import}
           (:violating-constructor-ids
            (outcome r :request/version-at-most-three))))))

(deftest independent-invariants-can-fail-in-different-cases
  (let [c (-> catalog
              (field :request/new :request/version [:literal 8])
              (field :request/import :request/note [:literal "present"]))
        r (check! c)]
    (is (= #{[:initial-state :request/version-at-most-three]
             [:initial-state :request/note-absent]}
           (:violated-declared-case-initial-ids r)))
    (is (= #{:request/new}
           (:violating-constructor-ids
            (outcome r :request/version-at-most-three))))
    (is (= #{:request/import}
           (:violating-constructor-ids
            (outcome r :request/note-absent))))))

(deftest absent-optional-is-different-from-present-nil
  (let [a (check! catalog)
        b (check! (field catalog :request/import :request/note [:literal nil]))]
    (is (not (contains? (:constructed-state (case-result a :request/import))
                        :request/note)))
    (is (contains? (:constructed-state (case-result b :request/import))
                   :request/note))
    (is (nil? (get-in (case-result b :request/import)
                     [:constructed-state :request/note])))
    (is (= #{:request/import}
           (:violating-constructor-ids (outcome b :request/note-absent))))
    (is (not= (:fingerprint a) (:fingerprint b)))))

(deftest optional-present-empty-string-is-also-present
  (let [r (check! (field catalog :request/new :request/note [:literal ""]))]
    (is (= #{:request/new}
           (:violating-constructor-ids (outcome r :request/note-absent))))
    (is (= "" (get-in (case-result r :request/new)
                       [:constructed-state :request/note])))))

(deftest optional-flag-presence-is-tracked-in-evidence
  (let [a (check! catalog)
        b (check! (field catalog :request/new :request/flag [:literal false]))
        c (check! (field catalog :request/new :request/flag [:literal true]))]
    (is (not (contains? (:constructed-state (case-result a :request/new))
                        :request/flag)))
    (is (false? (get-in (case-result b :request/new)
                        [:constructed-state :request/flag])))
    (is (true? (get-in (case-result c :request/new)
                       [:constructed-state :request/flag])))
    (is (not= (:fingerprint a) (:fingerprint b)))
    (is (not= (:fingerprint b) (:fingerprint c)))))

(deftest a-single-case-outside-the-initial-condition-is-rejected
  (rejects! (field catalog :request/import :request/version [:literal -1])
            constructor-error :constructor-outside-initial-condition))

(deftest an-initial-state-with-claimed-status-is-not-a-valid-case
  (rejects! (field catalog :request/new :request/status [:literal :claimed])
            constructor-error :constructor-outside-initial-condition))

(deftest every-case-must-typecheck-even-when-it-is-not-used-in-any-invariant
  (rejects! (field catalog :request/import :request/flag [:literal "nope"])
            evaluation-error :wrong-binding-type))

(deftest incorrect-enum-member-is-rejected
  (rejects! (field catalog :request/import :request/status [:literal :deleted])
            evaluation-error :enum-value-outside-domain))

(deftest optional-nonnullable-flag-rejects-nil
  (rejects! (field catalog :request/new :request/flag [:literal nil])
            evaluation-error :nil-for-nonnullable-binding))

(deftest omitted-optional-field-needs-explicit-absent-instruction
  (rejects! (change-case catalog :request/import
                         #(update % :fields dissoc :request/note))
            constructor-error :wrong-keys))

(deftest omitted-required-field-is-rejected
  (rejects! (change-case catalog :request/new
                         #(update % :fields dissoc :request/status))
            constructor-error :wrong-keys))

(deftest invented-state-field-is-rejected
  (rejects! (field catalog :request/new :request/not-declared [:literal 7])
            constructor-error :wrong-keys))

(deftest unsupported-dynamic-initializer-is-not-silently-enumerated
  (rejects! (field catalog :request/import :request/version
                   [:read :input/actor])
            constructor-error :unsupported-constructor-expression))

(deftest catalog-must-have-exact-three-key-shape
  (doseq [extra [{:certificate :issued}
                 {:authority :all}
                 {:proved? true}
                 {:runtime-enforcement :established}
                 {:physical-initialization-coverage :proved}]]
    (rejects! (merge catalog extra) catalog-error :wrong-keys)))

(deftest catalog-result-cannot-be-reused-as-catalog-source
  (rejects! (check! catalog) catalog-error :wrong-keys))

(deftest catalog-must-be-a-plain-persistent-map
  (doseq [bad [nil [] #{} "catalog" (into (sorted-map) catalog)]]
    (rejects! bad catalog-error :expected-plain-map)))

(deftest unqualified-and-nonkeyword-catalog-ids-are-rejected
  (doseq [id [:catalog "request/catalog" nil 4 false]]
    (rejects! (assoc catalog :id id) catalog-error :invalid-catalog-id)))

(deftest missing-declared-id-is-rejected
  (let [e (rejects! (update catalog :constructors pop)
                    catalog-error :constructor-id-coverage-mismatch)]
    (is (= #{:request/import} (:missing e)))
    (is (= #{} (:undeclared e)))))

(deftest extra-undeclared-id-is-rejected
  (let [e (rejects! (update catalog :constructors conj
                            (assoc create-case :id :request/unexpected))
                    catalog-error :constructor-id-coverage-mismatch)]
    (is (= #{} (:missing e)))
    (is (= #{:request/unexpected} (:undeclared e)))))

(deftest removing-an-id-from-the-declaration-cannot-hide-its-case
  (let [e (rejects! (update catalog :declared-constructor-ids
                            disj :request/import)
                    catalog-error :constructor-id-coverage-mismatch)]
    (is (= #{:request/import} (:undeclared e)))))

(deftest adding-an-id-to-the-declaration-requires-a-case
  (let [e (rejects! (update catalog :declared-constructor-ids
                            conj :request/seed)
                    catalog-error :constructor-id-coverage-mismatch)]
    (is (= #{:request/seed} (:missing e)))))

(deftest duplicate-constructor-id-is-rejected-before-set-comparison
  (let [c (assoc catalog :constructors
                 [create-case (assoc import-case :id :request/new)])
        e (rejects! c catalog-error :duplicate-constructor-id)]
    (is (= #{:request/new} (:duplicates e)))))

(deftest triple-duplicate-is-rejected
  (let [c (assoc catalog :constructors
                 [create-case create-case (assoc import-case :id :request/new)])
        e (rejects! c catalog-error :duplicate-constructor-id)]
    (is (= #{:request/new} (:duplicates e)))))

(deftest even-identical-duplicate-cases-are-not-idempotent
  (rejects! (assoc catalog :constructors [create-case create-case])
            catalog-error :duplicate-constructor-id))

(deftest declared-ids-must-be-a-nonempty-set
  (doseq [bad [nil [] [:request/new :request/import] #{} "all" 42]]
    (rejects! (assoc catalog :declared-constructor-ids bad)
              catalog-error :invalid-declared-constructor-ids)))

(deftest each-declared-id-must-be-qualified
  (doseq [bad [:new "request/new" 12 nil]]
    (rejects! (assoc catalog :declared-constructor-ids
                     #{:request/new bad})
              catalog-error :invalid-declared-constructor-ids)))

(deftest the-case-sequence-must-be-a-nonempty-persistent-vector
  (doseq [bad [nil [] () #{} (list create-case import-case)
               (seq [create-case import-case])]]
    (rejects! (assoc catalog :constructors bad)
              catalog-error :invalid-constructor-sequence)))

(deftest each-case-must-be-an-exact-plain-two-key-map
  (doseq [bad [(assoc create-case :proved true)
               (assoc create-case :authority :all)
               (dissoc create-case :fields)
               (into (sorted-map) create-case)]]
    (rejects! (assoc catalog :constructors [bad import-case])
              catalog-error
              (if (instance? clojure.lang.PersistentTreeMap bad)
                :expected-plain-map :wrong-keys))))

(deftest the-catalog-rejects-malformed-case-id
  (doseq [bad [:new "request/new" 17 nil]]
    (rejects! (assoc catalog :constructors
                     [(assoc create-case :id bad) import-case])
              catalog-error :invalid-constructor-id)))

(deftest catalog-metadata-is-not-an-authority-channel
  (rejects! (with-meta catalog {:proof :accepted})
            catalog-error :metadata-not-allowed))

(deftest declared-id-set-metadata-is-not-an-authority-channel
  (rejects! (update catalog :declared-constructor-ids
                    with-meta {:all-initializers? true})
            catalog-error :metadata-not-allowed))

(deftest case-vector-metadata-is-not-an-authority-channel
  (rejects! (update catalog :constructors
                    with-meta {:checked? true})
            catalog-error :metadata-not-allowed))

(deftest individual-case-metadata-is-not-an-authority-channel
  (rejects! (update catalog :constructors
                    #(assoc % 0 (with-meta create-case {:authorized? true})))
            catalog-error :metadata-not-allowed))

(deftest no-more-than-thirty-two-constructors
  (let [ids (mapv #(keyword "request" (str "case-" %)) (range 33))
        c (assoc catalog
                 :declared-constructor-ids (set ids)
                 :constructors (mapv #(assoc create-case :id %) ids))]
    (rejects! c catalog-error :invalid-declared-constructor-ids)))

(deftest a-catalog-may-have-exactly-thirty-two-declared-cases
  (let [ids (mapv #(keyword "request" (str "case-" %)) (range 32))
        c (assoc catalog
                 :declared-constructor-ids (set ids)
                 :constructors (mapv #(assoc create-case :id %) ids))
        r (check! c)]
    (is (= 32 (count (:case-results r))))
    (is (= 32 (count (:declared-constructor-ids r))))
    (is (true? (:all-declared-cases-satisfy-invariants? r)))))

(deftest a-constructor-source-change-invalidates-catalog-fingerprint
  (let [a (check! catalog)
        s (assoc-in source [:construction :replacements :request/status]
                    [:literal :done])
        b (check! s catalog)]
    (is (not= (:fingerprint a) (:fingerprint b)))
    (is (not= (:transition-fingerprint a) (:transition-fingerprint b)))
    (is (not= (:obligation-fingerprint a) (:obligation-fingerprint b)))))

(deftest a-guard-change-invalidates-catalog-fingerprint
  (let [a (check! catalog)
        s (assoc-in source [:construction :guard :expression]
                    [:in [:read :request/status] [:literal #{:open :claimed}]])
        b (check! s catalog)]
    (is (not= (:fingerprint a) (:fingerprint b)))
    (is (not= (:obligation-fingerprint a) (:obligation-fingerprint b)))))

(deftest an-initial-condition-change-invalidates-catalog-fingerprint
  (let [a (check! catalog)
        s (assoc source :initial-condition
                 {:readers #{:request/status}
                  :expression [:= [:read :request/status] [:literal :open]]})
        b (check! s catalog)]
    (is (not= (:fingerprint a) (:fingerprint b)))
    (is (not= (:obligation-fingerprint a) (:obligation-fingerprint b)))))

(deftest a-declared-invariant-change-invalidates-catalog-fingerprint
  (let [a (check! catalog)
        s (update source :invariants dissoc :request/version-at-most-three)
        b (check! s catalog)]
    (is (not= (:fingerprint a) (:fingerprint b)))
    (is (not= (:obligation-fingerprint a) (:obligation-fingerprint b)))
    (is (= (dec (count (:initial-catalog-outcomes a)))
           (count (:initial-catalog-outcomes b))))))

(deftest a-subject-identity-change-invalidates-catalog-fingerprint
  (let [a (check! catalog)
        s (assoc-in source [:construction :subject :id] :example/new-claim)
        b (check! s catalog)]
    (is (not= (:fingerprint a) (:fingerprint b)))
    (is (not= (:subject-fingerprint a) (:subject-fingerprint b)))))

(deftest a-forged-source-cannot-be-used-instead-of-original-source
  (doseq [forgery [{:proved? true}
                   {:authority :all}
                   {:certificate :issued}
                   {:all-initial-states-checked? true}]]
    (rejects! (merge source forgery) catalog
              obligation-error :wrong-keys)))

(deftest a-copied-obligation-report-cannot-be-used-as-source
  (rejects! (obligation/derive-obligations! source) catalog
            obligation-error :wrong-keys))

(deftest a-replaced-obligation-checker-cannot-forge-a-success
  (let [original obligation/derive-obligations!]
    (with-redefs [obligation/derive-obligations!
                  (fn [s] (assoc (original s) :authority :all))]
      (rejects! catalog catalog-error :obligation-checker-contract-changed))))

(deftest a-replaced-obligation-checker-cannot-drop-initial-obligations
  (let [original obligation/derive-obligations!]
    (with-redefs [obligation/derive-obligations!
                  (fn [s]
                    (update-in (original s) [:obligations :initial-state] pop))]
      (rejects! catalog catalog-error :obligation-checker-contract-changed))))

(deftest a-replaced-constructor-checker-cannot-forge-success
  (let [original constructor/check-initial-constructor!
        c (field catalog :request/import :request/version [:literal 4])]
    (with-redefs [constructor/check-initial-constructor!
                  (fn [s case]
                    (let [r (original s case)]
                      (if (= (:id case) :request/import)
                        (-> r
                            (assoc :all-declared-invariants-hold? true
                                   :violated-initial-case-ids #{}
                                   :checked-initial-case-ids (initial-ids s))
                            (update :initial-case-outcomes
                                    (fn [xs]
                                      (mapv (fn [x]
                                              (assoc x :value true
                                                     :status :checked-constructor-case
                                                     :reason nil)) xs))))
                        r)))]
      (rejects! c catalog-error :constructor-checker-contract-changed))))

(deftest a-replaced-constructor-checker-cannot-delete-an-obligation
  (let [original constructor/check-initial-constructor!]
    (with-redefs [constructor/check-initial-constructor!
                  (fn [s case]
                    (update (original s case) :initial-case-outcomes pop))]
      (rejects! catalog catalog-error :constructor-checker-contract-changed))))

(deftest a-replaced-constructor-checker-cannot-fabricate-authority
  (let [original constructor/check-initial-constructor!]
    (with-redefs [constructor/check-initial-constructor!
                  (fn [s case]
                    (assoc (original s case) :authority :commit))]
      (rejects! catalog catalog-error :constructor-checker-contract-changed))))

(deftest a-replaced-constructor-checker-cannot-forge-case-fingerprint
  (let [original constructor/check-initial-constructor!]
    (with-redefs [constructor/check-initial-constructor!
                  (fn [s case]
                    (assoc (original s case)
                           :fingerprint (str "sha256:" (apply str (repeat 64 "0")))))]
      (rejects! catalog catalog-error :constructor-checker-contract-changed))))

(deftest a-replaced-evaluator-cannot-forge-an-invariant-result
  (let [original rule-eval/evaluate-rule!
        c (field catalog :request/import :request/version [:literal 4])]
    (with-redefs [rule-eval/evaluate-rule!
                  (fn [decl types bindings]
                    (assoc (original decl types bindings) :value true))]
      (rejects! c constructor-error :rule-evaluator-contract-changed))))

(deftest finite-independent-oracle-over-versions-and-presence
  (doseq [v [0 1 3 4 12N 999999999999999999N]
          note [[:absent] [:literal nil] [:literal "memo"]]]
    (let [c (-> catalog
                (field :request/new :request/version [:literal v])
                (field :request/import :request/note note))
          r (check! c)
          v-ok? (<= v 3)
          note-ok? (= note [:absent])]
      (is (= v-ok?
             (= :checked-across-declared-cases
                (:status (outcome r :request/version-at-most-three)))))
      (is (= note-ok?
             (= :checked-across-declared-cases
                (:status (outcome r :request/note-absent)))))
      (is (= (cond-> #{}
               (not v-ok?) (conj [:initial-state :request/version-at-most-three])
               (not note-ok?) (conj [:initial-state :request/note-absent]))
             (:violated-declared-case-initial-ids r)))
      (is (= (not (or (not v-ok?) (not note-ok?)))
             (:all-declared-cases-satisfy-invariants? r))))))

(deftest catalog-evidence-remains-strictly-non-authoritative
  (let [r (check! catalog)]
    (is (= :original-declared-constructors-and-original-typed-rules
           (:checking r)))
    (is (= :finite-literal-initial-case-enumeration (:proof-rule r)))
    (is (= :finite-declared-literal-constructor-cases-only (:scope r)))
    (is (= :every-declared-constructor-case-evaluated (:claim r)))
    (is (= :conditional-declared-initial-case-coverage-checked
           (:proof-status r)))
    (is (= :not-established (:external-constructor-catalog-completeness r)))
    (is (= :not-established (:physical-initialization-coverage r)))
    (is (= :not-independently-proved (:checker-soundness r)))
    (is (= #{:external-constructor-catalog-completeness
             :physical-initialization-coverage
             :initial-rule-evaluation-soundness
             :certificate-admission}
           (:open-meta-obligations r)))
    (is (= :not-issued (:certificate r)))
    (is (= :not-admitted (:admission r)))
    (is (= :not-established (:runtime-enforcement r)))
    (is (= :not-established (:implementation-correspondence r)))
    (is (= :none (:authority r)))
    (is (= (all-open source) (:original-open-obligation-ids r)))
    (is (contains? (:original-open-obligation-ids r)
                   [:assumption :initial-state-existence]))
    (is (contains? (:original-open-obligation-ids r)
                   [:assumption :initial-state-coverage]))
    (is (not (contains? r :valid?)))
    (is (not (contains? r :proof-certificate)))
    (is (not (contains? r :transaction-authority)))))

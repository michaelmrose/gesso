(ns gesso.assurance.proof.initial-state-coverage-soundness-test
  "Permanent v1365 adversarial regression of the independent v1364 literal
   initializer image-to-induction-base correspondence. The finite image and
   induction schemas are conditional SOURCE-DECLARED facts, not assertions
   that physical initialization paths or deployed writers are complete.

   The suite deliberately separates: image subset from image equality;
   typed source construction from matching untrusted observations; and
   conditional inductive premises from runtime authority."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.edn :as edn]
            [gesso.assurance.proof.initial-state-coverage-soundness :as cov]
            [gesso.assurance.proof.abstract-execution-induction :as induction]
            [gesso.assurance.proof.typed-value-semantics :as typed]
            [gesso.assurance.proof.abstract-execution-induction-test :as induction-fixture]))

(def base induction-fixture/base)
(def state induction-fixture/state)
(defn- constructor [id state-types target]
  {:id id
   :fields (into {}
                 (map (fn [field]
                        [field (if (contains? target field)
                                 [:literal (get target field)]
                                 [:absent])]))
                 (keys state-types))})
(defn- initializers
  ([entries]
   {:id :initializers/request
    :declared-initializer-ids (set (map :id entries))
    :initializers (vec entries)})
  ([catalog entries]
   (assoc (initializers entries) :id catalog)))
(def one (constructor :initializers/one (:state-types base) state))
(def initial (initializers [one]))
(defn- coverage
  ([ics] (cov/derive-initial-coverage! base ics))
  ([ind ics] (cov/derive-initial-coverage! ind ics)))
(defn- witness
  ([id observed] (cov/audit-initializer-witness! base initial id observed))
  ([ind ics id observed]
   (cov/audit-initializer-witness! ind ics id observed)))
(defn- failure [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo e (ex-data e))))
(defn- rejected? [f] (some? (failure f)))
(defn- no-authority! [report]
  (is (= :not-issued (:certificate report)))
  (is (= :not-admitted (:admission report)))
  (is (= :none (:authority report)))
  (is (= :not-established (:real-initializer-coverage report)))
  (is (= :not-established (:initializer-catalog-completeness report)))
  (is (not= :proved (:proof-status report))))
(defn- error-reason [f] (:reason (failure f)))

(deftest v1365-exact-image-report-contract
  (let [r (coverage initial)]
    (is (= cov/report-type (:type r)))
    (is (= cov/semantics-version (:version r)))
    (is (= base (:source-induction-catalog r)))
    (is (= initial (:source-initializer-catalog r)))
    (is (= (:id base) (:induction-catalog-id r)))
    (is (= (:id initial) (:initializer-catalog-id r)))
    (is (= #{:initializers/one} (:declared-initializer-ids r)))
    (is (= [{:initializer-id :initializers/one :constructed-state state}]
           (:constructor-results r)))
    (is (= #{state} (:constructor-image r)))
    (is (= #{state} (:declared-base-states r)))
    (is (= #{} (:outside-declared-base-states r)))
    (is (= #{} (:unrepresented-enumerated-base-states r)))
    (is (true? (:all-declared-initializers-in-base? r)))
    (is (true? (:exact-declared-initializer-image? r)))
    (is (= :conditional-exact-declared-image (:status r)))
    (is (= :every-declared-literal-constructor-produces-declared-base
           (:conditional-coverage-claim r)))
    (is (= :declared-literal-initializer-and-declared-history-preserve-invariant
           (:conditional-history-claim r)))
    (is (= :conditional-declared-induction-schema (:induction-schema-status r)))
    (is (= :conditional-source-schema-only (:proof-status r)))
    (is (= :finite-source-declared-literal-initializers-only (:scope r)))
    (no-authority! r)))

(deftest report-has-explicit-unresolved-external-obligations
  (let [r (coverage initial)]
    (is (every? (:external-obligations r)
                [:actual-initializer-id-completeness
                 :physical-initializer-to-literal-constructor-refinement
                 :all-real-initialization-paths-mediated
                 :declared-state-projection-completeness
                 :sound-independent-typed-state-semantics
                 :universal-induction-implementation-soundness
                 :xtdb-initialization-correspondence
                 :certificate-admission]))
    (is (= :not-established (:physical-initialization-refinement r)))
    (is (= :not-established (:universal-implementation-soundness r)))
    (is (= :not-established (:runtime-enforcement r)))
    (is (= r (edn/read-string (pr-str r))))))

(deftest subset-of-enumerated-base-is-sufficient
  (let [extra (assoc state :request/owner :user/extra)
        ind (assoc base :declared-initial-states [state extra])
        r (coverage ind initial)]
    (is (= :conditional-constructor-image-subset (:status r)))
    (is (= #{state} (:constructor-image r)))
    (is (= #{extra} (:unrepresented-enumerated-base-states r)))
    (is (empty? (:outside-declared-base-states r)))
    (is (true? (:all-declared-initializers-in-base? r)))
    (is (false? (:exact-declared-initializer-image? r)))
    (is (some? (:conditional-coverage-claim r)))
    (is (some? (:conditional-history-claim r)))
    (no-authority! r)))

(deftest reversed-image-subset-is-not-sufficient
  (let [extra (assoc state :request/owner :user/extra)
        ics (initializers [one (constructor :initializers/two (:state-types base) extra)])
        r (coverage ics)]
    (is (= :open-constructor-outside-base (:status r)))
    (is (= #{extra} (:outside-declared-base-states r)))
    (is (false? (:all-declared-initializers-in-base? r)))
    (is (nil? (:conditional-coverage-claim r)))
    (is (nil? (:conditional-history-claim r)))
    (no-authority! r)))

(deftest image-coverage-and-duplicate-state-images
  (let [second-entry (assoc one :id :initializers/alias)
        ics (initializers [second-entry one])
        r (coverage ics)]
    (is (= :conditional-exact-declared-image (:status r)))
    (is (= 2 (count (:constructor-results r))))
    (is (= 1 (count (:constructor-image r))))
    (is (= [:initializers/alias :initializers/one]
           (mapv :initializer-id (:constructor-results r))))
    (is (= [:initializers/alias :initializers/one]
           (mapv :initializer-id (:constructor-results (coverage (update ics :initializers #(vec (reverse %))))))))))

(deftest multiple-images-enumerated-out-of-order
  (let [a (assoc state :request/owner :user/a)
        b (assoc state :request/owner :user/b)
        c (assoc state :request/owner nil)
        ind (assoc base :declared-initial-states [c b a])
        entries [(constructor :initializers/z (:state-types base) c)
                 (constructor :initializers/a (:state-types base) a)
                 (constructor :initializers/m (:state-types base) b)]
        r (coverage ind (initializers entries))]
    (is (= :conditional-exact-declared-image (:status r)))
    (is (= #{a b c} (:constructor-image r)))
    (is (= [:initializers/a :initializers/m :initializers/z]
           (mapv :initializer-id (:constructor-results r))))))

(deftest typed-constructor-preserves-large-exact-integer
  (let [huge 99999999999999999999999999999N
        target (assoc state :request/version huge)
        ind (assoc base :declared-initial-states [target])
        ics (initializers [(constructor :initializers/big (:state-types base) target)])
        r (coverage ind ics)]
    (is (= :conditional-exact-declared-image (:status r)))
    (is (= huge (get-in r [:constructor-results 0 :constructed-state :request/version])))))

(deftest required-present-nil-is-not-absent
  (let [target (assoc state :request/owner nil)
        ind (assoc base :declared-initial-states [target])
        ics (initializers [(constructor :initializers/nil (:state-types base) target)])
        r (coverage ind ics)]
    (is (= :conditional-exact-declared-image (:status r)))
    (is (contains? (get-in r [:constructor-results 0 :constructed-state]) :request/owner))
    (is (nil? (get-in r [:constructor-results 0 :constructed-state :request/owner])))))

(deftest optional-absence-distinct-from-present-nil
  (let [absent (dissoc state :request/note)
        nil-note (assoc state :request/note nil)
        ind (assoc base :declared-initial-states [absent nil-note])
        ics (initializers [(constructor :initializers/absent (:state-types base) absent)
                           (constructor :initializers/nil-note (:state-types base) nil-note)])
        r (coverage ind ics)]
    (is (= :conditional-exact-declared-image (:status r)))
    (is (not= absent nil-note))
    (is (= #{absent nil-note} (:constructor-image r)))
    (is (not (contains? absent :request/note)))
    (is (contains? nil-note :request/note))))

(deftest optional-absence-not-interchangeable-with-nil-base
  (let [absent (dissoc state :request/note)
        ics (initializers [(constructor :initializers/absent (:state-types base) absent)])
        r (coverage ics)]
    (is (= :open-constructor-outside-base (:status r)))
    (is (= #{absent} (:outside-declared-base-states r)))
    (is (= #{state} (:unrepresented-enumerated-base-states r)))))

(deftest optional-nil-constructor-not-interchangeable-with-absent-base
  (let [absent (dissoc state :request/note)
        nil-note (assoc state :request/note nil)
        ind (assoc base :declared-initial-states [absent])
        ics (initializers [(constructor :initializers/nil (:state-types base) nil-note)])
        r (coverage ind ics)]
    (is (= :open-constructor-outside-base (:status r)))
    (is (= #{nil-note} (:outside-declared-base-states r)))
    (is (= #{absent} (:unrepresented-enumerated-base-states r)))))

(deftest unsupported-initial-guard-leaves-history-claim-open
  (let [bad-base (assoc state :request/status :open)
        ind (assoc base :declared-initial-states [bad-base])
        ics (initializers [(constructor :initializers/open (:state-types base) bad-base)])
        r (coverage ind ics)]
    (is (= :conditional-exact-declared-image (:status r)))
    (is (= :open-base-or-step (:induction-schema-status r)))
    (is (= #{0} (:failing-initial-indices r)))
    (is (nil? (:conditional-history-claim r)))
    (is (some? (:conditional-coverage-claim r)))
    (no-authority! r)))

(deftest unsupported-operation-leaves-history-open-with-exact-image
  (let [        ind (assoc base :invariant
                   {:id :initializers/open-ownership
                    :readers #{:request/version :request/status :request/owner}
                    :expression [:and [:>= [:read :request/version] [:literal 0]]
                                 [:= [:read :request/status] [:literal :claimed]]
                                 [:not= [:read :request/owner] [:literal nil]]]})
        r (coverage ind initial)]
    (is (= :open-base-or-step (:induction-schema-status r)))
    (is (= :conditional-exact-declared-image (:status r)))
    (is (seq (:unsupported-operation-ids r)))
    (is (nil? (:conditional-history-claim r)))
    (no-authority! r)))

(deftest witness-for-matching-constructor
  (let [r (witness :initializers/one state)]
    (is (= cov/witness-type (:type r)))
    (is (= cov/semantics-version (:version r)))
    (is (= :initializers/one (:initializer-id r)))
    (is (= state (:observed-state r)))
    (is (= state (:expected-state r)))
    (is (true? (:matches-declared-constructor? r)))
    (is (true? (:history-premises-supported? r)))
    (is (= :matching-declared-initializer-witness (:status r)))
    (is (= :conditional-exact-declared-image (:coverage-status r)))
    (is (= :finite-witness-not-universal-proof (:proof-status r)))
    (is (= :one-untrusted-abstract-initializer-observation (:scope r)))
    (is (= :not-established (:physical-provenance r)))
    (no-authority! r)))

(deftest witness-mismatch-is-not-an-error-or-proof
  (let [observed (assoc state :request/owner :user/other)
        r (witness :initializers/one observed)]
    (is (= :nonmatching-declared-initializer-witness (:status r)))
    (is (false? (:matches-declared-constructor? r)))
    (is (false? (:history-premises-supported? r)))
    (is (= state (:expected-state r)))
    (is (= observed (:observed-state r)))
    (no-authority! r)))

(deftest witness-matching-while-other-initializer-outside-base
  (let [outside (assoc state :request/owner :user/outside)
        ics (initializers [one (constructor :initializers/outside (:state-types base) outside)])
        r (witness base ics :initializers/one state)]
    (is (true? (:matches-declared-constructor? r)))
    (is (= :matching-witness-but-initial-coverage-open (:status r)))
    (is (false? (:history-premises-supported? r)))
    (no-authority! r)))

(deftest witness-matching-while-induction-is-open
  (let [open-state (assoc state :request/status :open)
        ind (assoc base :declared-initial-states [open-state])
        ics (initializers [(constructor :initializers/open (:state-types base) open-state)])
        r (witness ind ics :initializers/open open-state)]
    (is (true? (:matches-declared-constructor? r)))
    (is (= :matching-witness-but-induction-open (:status r)))
    (is (false? (:history-premises-supported? r)))
    (no-authority! r)))

(deftest witness-subset-image-can-support-conditional-premises
  (let [other (assoc state :request/owner :user/other)
        ind (assoc base :declared-initial-states [state other])
        r (witness ind initial :initializers/one state)]
    (is (= :conditional-constructor-image-subset (:coverage-status r)))
    (is (= :matching-declared-initializer-witness (:status r)))
    (is (true? (:history-premises-supported? r)))
    (no-authority! r)))

(deftest undeclared-witness-id-is-rejected
  (is (= :undeclared-initializer-id
         (error-reason #(witness :initializers/unknown state)))))

(deftest witness-rejects-invalid-typed-observation
  (doseq [bad [(dissoc state :request/version)
               (assoc state :request/version "four")
               (assoc state :request/status :request/unknown)
               (assoc state :not-declared 5)
               (with-meta state {:trusted true})]]
    (is (rejected? #(witness :initializers/one bad)) (pr-str bad))))

(deftest report-source-and-image-recomputed-on-each-call
  (let [changed (assoc state :request/owner :user/changed)
        ics (initializers [(constructor :initializers/changed (:state-types base) changed)])
        before (coverage initial)
        after (coverage ics)]
    (is (= initial (:source-initializer-catalog before)))
    (is (= ics (:source-initializer-catalog after)))
    (is (not= (:constructor-image before) (:constructor-image after)))
    (is (= #{changed} (:outside-declared-base-states after)))
    (is (= #{state} (:constructor-image before)))))

(deftest invalid-catalog-keys-and-ids
  (doseq [bad [(assoc initial :extra :untrusted)
               (dissoc initial :id)
               (assoc initial :id :unqualified)
               (assoc initial :id "initializers/request")
               (with-meta initial {:trusted true})
               []
               {:id :initializers/request}]]
    (is (rejected? #(coverage bad)) (pr-str bad))))

(deftest declared-ids-must-be-nonempty-qualified-plain-set
  (doseq [bad [#{} #{:unqualified} #{:initializers/one :unqualified}
               [:initializers/one] '(:initializers/one)
               (with-meta #{:initializers/one} {:trusted true})]]
    (is (rejected? #(coverage (assoc initial :declared-initializer-ids bad)))
        (pr-str bad))))

(deftest initializer-entries-must-be-nonempty-plain-vector
  (doseq [bad [[] (list one) #{one}
               (with-meta [one] {:trusted true})]]
    (is (rejected? #(coverage (assoc initial :initializers bad)))
        (pr-str bad))))

(deftest initializer-id-catalog-must-match-entry-set-exactly
  (doseq [ics [(assoc initial :declared-initializer-ids #{:initializers/other})
               (assoc initial :declared-initializer-ids #{:initializers/one :initializers/missing})
               (assoc initial :initializers [one (assoc one :id :initializers/one)])
               (assoc initial :initializers [one (assoc one :id :initializers/extra)])]]
    (is (= :initializer-id-coverage-mismatch
           (error-reason #(coverage ics))) (pr-str ics))))

(deftest each-entry-requires-exact-plain-map-and-qualified-id
  (doseq [entry [(assoc one :extra :trusted)
                 (dissoc one :fields)
                 (assoc one :id :unqualified)
                 (assoc one :id "initializers/one")
                 (with-meta one {:trusted true})]]
    (let [ics (assoc initial :initializers [entry]
                     :declared-initializer-ids #{:initializers/one})]
      (is (rejected? #(coverage ics)) (pr-str entry)))))

(deftest every-projected-field-needs-exactly-one-expression
  (doseq [fields [(dissoc (:fields one) :request/status)
                 (assoc (:fields one) :field/extra [:literal 0])
                 (with-meta (:fields one) {:trusted true})]]
    (is (rejected? #(coverage (assoc-in initial [:initializers 0 :fields] fields)))
        (pr-str fields))))

(deftest required-fields-cannot-be-absent
  (doseq [field [:request/status :request/version :request/owner]]
    (is (= :absent-required-field
           (error-reason #(coverage (assoc-in initial
                                             [:initializers 0 :fields field]
                                             [:absent]))))
        (str field))))

(deftest optional-field-can-be-absent
  (let [absent (dissoc state :request/note)
        ind (assoc base :declared-initial-states [absent])
        r (coverage ind (initializers [(constructor :initializers/none
                                                   (:state-types base) absent)]))]
    (is (= :conditional-exact-declared-image (:status r)))
    (is (= #{absent} (:constructor-image r)))))

(deftest malformed-expression-vectors-rejected
  (doseq [expr [[:bogus "saved"] [:literal]
                [:literal "saved" :extra]
                [:absent nil]
                [:read :request/status]
                []
                '(:literal "saved")
                (with-meta [:literal "saved"] {:trusted true})]]
    (is (rejected? #(coverage (assoc-in initial
                                        [:initializers 0 :fields :request/note]
                                        expr))) (pr-str expr))))

(deftest absent-expression-is-not-equivalent-to-literal-nil
  (let [nil-ics (assoc-in initial [:initializers 0 :fields :request/note]
                          [:literal nil])
        absent-ics (assoc-in initial [:initializers 0 :fields :request/note]
                             [:absent])
        nil-report (coverage nil-ics)
        absent-report (coverage absent-ics)]
    (is (= :open-constructor-outside-base (:status nil-report)))
    (is (= :open-constructor-outside-base (:status absent-report)))
    (is (not= (:constructor-image nil-report)
              (:constructor-image absent-report)))
    (is (= #{(assoc state :request/note nil)} (:constructor-image nil-report)))
    (is (= #{(dissoc state :request/note)} (:constructor-image absent-report)))))

(deftest wrong-type-and-unknown-enum-fail-closed
  (doseq [[field value] [[:request/status :not-in-domain]
                         [:request/status 0]
                         [:request/version "4"]
                         [:request/owner 17]
                         [:request/note 17]
                         [:request/status nil]
                         [:request/version 1.5]]]
    (is (rejected? #(coverage (assoc-in initial
                                        [:initializers 0 :fields field]
                                        [:literal value])))
        (pr-str [field value]))))

(deftest executable-and-container-values-fail-closed
  (doseq [value [(fn [] :claimed)
                 {:exec '(java.lang.System/exit 0)}
                 [:literal :claimed]
                 #{:claimed}]]
    (is (rejected? #(coverage (assoc-in initial
                                        [:initializers 0 :fields :request/status]
                                        [:literal value]))))))

(deftest bounded-initializer-catalog-exactly-thirty-two
  (let [states (mapv #(assoc state :request/version (bigint %)) (range 32))
        ind (assoc base :declared-initial-states states)
        entries (mapv #(constructor (keyword "initializers" (str "v" %))
                                    (:state-types base) (nth states %))
                      (range 32))
        r (coverage ind (initializers entries))]
    (is (= 32 (count (:constructor-results r))))
    (is (= 32 (count (:constructor-image r))))
    (is (= :conditional-exact-declared-image (:status r)))
    (is (= #{} (:outside-declared-base-states r)))
    (no-authority! r)))

(deftest thirty-three-initializers-rejected
  (let [entries (mapv #(assoc one :id (keyword "initializers" (str "v" %)))
                      (range 33))]
    (is (= :invalid-declared-initializer-set
           (error-reason #(coverage (initializers entries)))))))

(deftest unequal-image-bases-reported-both-ways
  (let [a (assoc state :request/version 9)
        b (assoc state :request/version 10)
        c (assoc state :request/version 11)
        ind (assoc base :declared-initial-states [state a])
        ics (initializers [one (constructor :initializers/b (:state-types base) b)
                           (constructor :initializers/c (:state-types base) c)])
        r (coverage ind ics)]
    (is (= #{b c} (:outside-declared-base-states r)))
    (is (= #{a} (:unrepresented-enumerated-base-states r)))
    (is (= :open-constructor-outside-base (:status r)))
    (is (nil? (:conditional-history-claim r)))))

(deftest forged-public-induction-checker-is-not-trusted
  (with-redefs [induction/derive-induction-schema!
                (fn [& _] {:status :conditional-declared-induction-schema
                           :proof-status :proved :authority :root})]
    (is (= :substituted-induction-schema
           (error-reason #(coverage initial))))))

(deftest forged-public-typed-state-interpreter-is-not-trusted
  (with-redefs [typed/observe-state!
                (fn [& _] {:fields {} :proof-status :proved :authority :root})]
    (is (rejected? #(coverage initial)))))

(deftest metadata-on-initializer-source-does-not-create-trust
  (let [replaced (assoc-in initial [:initializers 0 :fields :request/note]
                           (with-meta [:literal "saved"] {:proof :authority}))]
    (is (= :metadata-not-allowed
           (error-reason #(coverage replaced))))))

(deftest diagnostic-result-serialization-does-not-gain-authority
  (doseq [r [(coverage initial)
             (witness :initializers/one state)
             (witness :initializers/one (assoc state :request/owner :user/other))]]
    (is (= r (edn/read-string (pr-str r))))
    (no-authority! r)))

(deftest constructor-observation-is-never-physical-provenance
  (let [r (witness :initializers/one state)]
    (is (true? (:history-premises-supported? r)))
    (is (= :not-established (:physical-provenance r)))
    (is (= :not-established (:real-initializer-coverage r)))
    (is (= :not-issued (:certificate r)))
    (is (= :none (:authority r)))))

(deftest missing-operation-in-declared-catalog-does-not-prove-real-completeness
  (let [r (coverage initial)]
    (is (= (:declared-operation-ids base)
           (:declared-operation-ids
            (induction/derive-induction-schema! base))))
    (is (contains? (:external-obligations r)
                   :all-real-initialization-paths-mediated))
    (is (= :not-established (:initializer-catalog-completeness r)))
    (is (= :not-established (:physical-initialization-refinement r)))))

(deftest source-preservation-of-ids-and-nil-vs-absent-after-roundtrip
  (let [target (assoc (dissoc state :request/note) :request/owner nil)
        ind (assoc base :declared-initial-states [target])
        ics (initializers [(constructor :initializers/nullable (:state-types base) target)])
        r (coverage ind ics)
        decoded (edn/read-string (pr-str r))]
    (is (= :conditional-exact-declared-image (:status r)))
    (is (= ind (:source-induction-catalog decoded)))
    (is (= ics (:source-initializer-catalog decoded)))
    (is (contains? (first (:constructor-image decoded)) :request/owner))
    (is (not (contains? (first (:constructor-image decoded)) :request/note)))
    (no-authority! decoded)))

(deftest finite-typed-constructor-image-agrees-with-direct-state-oracle
  ;; A separate finite oracle computes 27 distinct states from orthogonal
  ;; type-valid dimensions. The source-derived image must be exactly equal,
  ;; not merely of the same cardinality. All satisfy the declared invariant.
  (let [states (vec (for [v [0N 1N 9N]
                          owner [nil :user/a :user/b]
                          note [:absent nil "saved"]]
                      (cond-> (assoc state :request/version v
                                          :request/owner owner)
                        (= note :absent) (dissoc :request/note)
                        (not= note :absent) (assoc :request/note note))))
        ind (assoc base :declared-initial-states (vec (reverse states)))
        entries (mapv (fn [index s]
                        (constructor (keyword "initializers" (str "grid-" index))
                                     (:state-types base) s))
                      (range (count states)) states)
        expected-by-id (into {} (map (fn [entry s] [(:id entry) s])
                                      entries states))
        r (coverage ind (initializers (vec (reverse entries))))]
    (is (= 27 (count states)))
    (is (= 27 (count (set states))))
    (is (= 27 (count (:constructor-results r))))
    (is (= (set states) (:constructor-image r)))
    (is (= :conditional-exact-declared-image (:status r)))
    (is (= :conditional-declared-induction-schema (:induction-schema-status r)))
    (is (empty? (:outside-declared-base-states r)))
    (doseq [row (:constructor-results r)]
      (is (contains? (set states) (:constructed-state row)))
      (is (= (get expected-by-id (:initializer-id row))
             (:constructed-state row))))
    (no-authority! r)))

(ns gesso.assurance.proof.frame-soundness-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.edn :as edn]
            [gesso.assurance.proof.frame-soundness :as frame]
            [gesso.assurance.proof.independent-transition-semantics :as independent]
            [gesso.assurance.proof.transition :as production]))

(def claim
  {:subject {:kind :operation :id :example/claim :interface {:operation :example/claim}}
   :state-types {:request/status {:type :enum :name :request/status
                                  :values #{:open :claimed}}
                 :request/owner {:type :keyword :nullable? true}
                 :request/version {:type :integer}
                 :request/note {:type :string :optional? true :nullable? true}}
   :input-types {:input/actor {:type :keyword}}
   :guard {:readers #{:request/status :request/owner}
           :expression [:and [:= [:read :request/status] [:literal :open]]
                        [:= [:read :request/owner] [:literal nil]]]}
   :reads #{:request/status :request/owner :input/actor}
   :writes #{:request/status :request/owner}
   :assignments {:request/status [:literal :claimed]
                 :request/owner [:read :input/actor]}})
(def pre {:request/status :open :request/owner nil :request/version 7})
(def inp {:input/actor :user/a})
(def post {:request/status :claimed :request/owner :user/a :request/version 7})
(def stable-ver {:id :example/version-positive
                 :readers #{:request/version}
                 :expression [:>= [:read :request/version] [:literal 0]]})
(def stable-note {:id :example/note-nonblank
                  :readers #{:request/note}
                  :expression [:or [:not [:present [:read :request/note]]]
                               [:and [:not= [:read :request/note] [:literal nil]]
                                [:not= [:read :request/note] [:literal ""]]]]})
(def written-owner {:id :example/owner-nil
                    :readers #{:request/owner}
                    :expression [:= [:read :request/owner] [:literal nil]]})
(def stable-constant {:id :example/true :readers #{}
                      :expression [:literal true]})
(defn rejects? [f] (try (f) false (catch clojure.lang.ExceptionInfo _ true)))

(deftest v1353-static-frame-rule
  (let [proof (frame/derive-frame-lemma! claim stable-ver)]
    (is (= :conditional-frame-rule-instance (:status proof)))
    (is (= #{:request/version :request/note} (:frame-fields proof)))
    (is (= #{:request/version} (:readers proof)))
    (is (empty? (:written-readers proof)))
    (is (= (:fingerprint (production/describe-transition! claim))
           (:transition-fingerprint proof)))
    (is (= :not-independently-proved (:typed-expression-soundness proof)))
    (is (= :not-issued (:certificate proof)))
    (is (= :none (:authority proof)))))

(deftest v1353-written-field-is-open
  (let [proof (frame/derive-frame-lemma! claim written-owner)
        observed (frame/audit-frame-witness! claim written-owner pre inp post)]
    (is (= :open-written-reader (:status proof)))
    (is (= #{:request/owner} (:written-readers proof)))
    (is (= :open-written-reader (:status observed)))
    (is (true? (:matches-transition? observed)))
    (is (false? (:after-invariant observed)))
    (is (false? (:preservation-implication-holds? observed)))))

(deftest v1353-positive-matching-witness
  (let [o (frame/audit-frame-witness! claim stable-ver pre inp post)]
    (is (= :checked-conditional-frame-instance (:status o)))
    (is (:matches-transition? o))
    (is (:reader-projections-equal? o))
    (is (:before-invariant o))
    (is (:after-invariant o))
    (is (:preservation-implication-holds? o))
    (is (= :not-universally-proved (:proof-status o)))
    (is (= :not-established (:physical-observation o)))))

(deftest v1353-false-precondition-not-an-invariant-proof
  (let [bad-pre (assoc pre :request/version -3)
        bad-post (assoc post :request/version -3)
        o (frame/audit-frame-witness! claim stable-ver bad-pre inp bad-post)]
    (is (= :checked-conditional-frame-instance (:status o)))
    (is (false? (:before-invariant o)))
    (is (false? (:after-invariant o)))
    (is (true? (:preservation-implication-holds? o)))))

(deftest v1353-null-and-absence-are-distinct
  (doseq [note [::absent nil "memo" ""]]
    (let [b (cond-> pre (not= note ::absent) (assoc :request/note note))
          a (cond-> post (not= note ::absent) (assoc :request/note note))
          result (frame/audit-frame-witness! claim stable-note b inp a)]
      (is (= :checked-conditional-frame-instance (:status result)))
      (is (= (:before-invariant result) (:after-invariant result)))
      (is (= (or (= note ::absent) (and (some? note) (not= note "")))
             (:before-invariant result))))))

(deftest v1353-frame-presence-change-invalidates-relation
  (doseq [[b a] [[pre (assoc post :request/note nil)]
                 [(assoc pre :request/note nil) post]
                 [(assoc pre :request/note "memo") (assoc post :request/note "edited")]
                 [pre (assoc post :request/version 8)]]]
    (let [r (frame/audit-frame-witness! claim stable-ver b inp a)]
      (is (= :nonmatching-transition (:status r)))
      (is (false? (:matches-transition? r))))))

(deftest v1353-wrong-guard-nonmatching
  (let [b (assoc pre :request/status :claimed)
        r (frame/audit-frame-witness! claim stable-ver b inp post)]
    (is (= :nonmatching-transition (:status r)))
    (is (false? (:matches-transition? r)))))

(deftest v1353-constant-invariant
  (let [l (frame/derive-frame-lemma! claim stable-constant)
        r (frame/audit-frame-witness! claim stable-constant pre inp post)]
    (is (= :conditional-frame-rule-instance (:status l)))
    (is (empty? (:readers l)))
    (is (= :checked-conditional-frame-instance (:status r)))))

(deftest v1353-unrelated-guard-reads-do-not-matter
  (is (= #{:request/version :request/note}
         (:frame-fields (frame/derive-frame-lemma! claim stable-ver)))))

(deftest v1353-reject-hidden-written-reader
  (doseq [bad [(assoc stable-ver :readers #{})
               (assoc written-owner :readers #{:request/version})
               (assoc stable-ver :readers #{:request/version :request/owner})]]
    (is (rejects? #(frame/derive-frame-lemma! claim bad)))))

(deftest v1353-reject-unadmitted-invariant-source
  (doseq [bad [(assoc stable-ver :extra/a true)
               (assoc stable-ver :id :bad)
               (assoc stable-ver :readers [:request/version])
               (assoc stable-ver :expression [:foo [:read :request/version]])
               (assoc stable-ver :expression [:= [:read :input/actor] [:literal :user/a]])
               (assoc stable-ver :expression [:and [:read :request/version]])
               (assoc stable-ver :expression [:not])
               (with-meta stable-ver {:trust true})
               (assoc stable-ver :expression
                      (with-meta [:literal true] {:trust true}))]]
    (is (rejects? #(frame/derive-frame-lemma! claim bad)) (pr-str bad))))

(deftest v1353-transition-rejection-is-not-a-proof
  (doseq [bad [(assoc claim :reads #{:request/status})
               (assoc claim :writes #{:request/owner})
               (assoc-in claim [:assignments :request/status] [:fn inc])]]
    (is (rejects? #(frame/derive-frame-lemma! bad stable-ver)))))

(deftest v1353-synthetic-forged-independent-observation
  (with-redefs [independent/observe-transition!
                (fn [& _] {:matches? true})]
    (is (rejects? #(frame/audit-frame-witness!
                    claim stable-ver pre inp (assoc post :request/version 8))))))

(deftest v1353-checked-instance-is-copyable-untrusted-data
  (let [report (frame/audit-frame-witness! claim stable-ver pre inp post)]
    (is (= :not-issued (:certificate report)))
    (is (= :not-admitted (:admission report)))
    (is (= :none (:authority report)))
    (is (= report (clojure.edn/read-string (pr-str report))))))

(deftest v1353-type-admission-precedes-frame-eligibility
  ;; Syntactic disjointness alone must not approve a non-Boolean invariant.
  (is (rejects? #(frame/derive-frame-lemma!
                  claim {:id :example/not-a-predicate
                         :readers #{} :expression [:literal 7]})))
  (is (rejects? #(frame/derive-frame-lemma!
                  claim {:id :example/type-error
                         :readers #{:request/version}
                         :expression [:> [:read :request/version]
                                         [:literal "not-an-int"]]}))))

(deftest v1353-source-rebinding-invalidate-lemma
  (let [original (frame/derive-frame-lemma! claim stable-ver)
        changed-source (assoc-in claim [:assignments :request/status]
                                 [:literal :open])
        modified (frame/derive-frame-lemma! changed-source stable-ver)
        changed-invariant (assoc stable-ver :expression
                                 [:>= [:read :request/version] [:literal 10]])
        changed-rule (frame/derive-frame-lemma! claim changed-invariant)]
    (is (not= (:transition-fingerprint original)
              (:transition-fingerprint modified)))
    (is (not= (:typed-invariant-fingerprint original)
              (:typed-invariant-fingerprint changed-rule)))
    (is (= :conditional-frame-rule-instance (:status modified)))
    (is (= :conditional-frame-rule-instance (:status changed-rule)))))

;; v1354: Permanent adversarial coverage. These checks are intentionally
;; limited to the conditional, source-relative frame lemma; neither a single
;; witness nor a serializable report proves production interpreter soundness.

(defn- exception-reason [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo ex
         (or (:reason (ex-data ex)) ::missing-reason))))

(deftest v1354-premises-and-no-certificate-for-every-success
  (doseq [invariant [stable-ver stable-note stable-constant]]
    (let [r (frame/derive-frame-lemma! claim invariant)]
      (is (= :conditional-frame-rule-instance (:status r)))
      (is (= :conditional-schema-only (:proof-status r)))
      (is (= :matching-transition-preserves-invariant-truth
             (:conditional-claim r)))
      (is (= #{:valid-typed-invariant
               :exact-abstract-transition-frame
               :pure-typed-expression-extensionality}
             (:premises r)))
      (is (= :not-independently-proved
             (:typed-expression-soundness r)))
      (is (= :not-independently-proved
             (:transition-implementation-soundness r)))
      (is (= :not-issued (:certificate r)))
      (is (= :not-admitted (:admission r)))
      (is (= :none (:authority r))))))

(deftest v1354-exact-footprints-do-not-trust-declared-readers
  (doseq [[invariant expected]
          [[stable-ver #{:request/version}]
           [stable-note #{:request/note}]
           [stable-constant #{}]
           [written-owner #{:request/owner}]]]
    (let [r (frame/derive-frame-lemma! claim invariant)]
      (is (= expected (:readers r)))
      (is (= (set (filter (:writes claim) expected))
             (:written-readers r)))))
  (doseq [bad [(assoc stable-note :readers #{})
               (assoc stable-note :readers #{:request/version})
               (assoc stable-constant :readers #{:request/owner})
               (assoc stable-ver :readers #{:request/version :request/note})]]
    (is (= :invariant-reader-footprint-mismatch
           (exception-reason #(frame/derive-frame-lemma! claim bad))))))

(deftest v1354-conditional-frame-eligibility-is-static
  ;; An invariant may read a written field only in an unreachable branch.
  ;; Its *syntactic* footprint still prevents a frame lemma.
  (let [hidden {:id :example/hidden-write
                :readers #{:request/owner}
                :expression [:or [:literal true]
                             [:= [:read :request/owner] [:literal nil]]]}
        r (frame/derive-frame-lemma! claim hidden)
        observed (frame/audit-frame-witness! claim hidden pre inp post)]
    (is (= :open-written-reader (:status r)))
    (is (= #{:request/owner} (:written-readers r)))
    (is (nil? (:conditional-claim r)))
    (is (= :open-written-reader (:status observed)))
    (is (true? (:before-invariant observed)))
    (is (true? (:after-invariant observed)))))

(deftest v1354-unchanged-value-of-written-reader-is-still-open
  (let [owner-not-b {:id :example/owner-not-b
                     :readers #{:request/owner}
                     :expression [:not= [:read :request/owner]
                                   [:literal :user/b]]}
        l (frame/derive-frame-lemma! claim owner-not-b)
        w (frame/audit-frame-witness! claim owner-not-b pre inp post)]
    (is (= :open-written-reader (:status l)))
    (is (= :open-written-reader (:status w)))
    (is (true? (:before-invariant w)))
    (is (true? (:after-invariant w)))
    (is (true? (:preservation-implication-holds? w)))
    (is (= :not-universally-proved (:proof-status w)))))

(deftest v1354-finite-valid-frame-grid
  ;; A matching claim can change owner/status, but not version or note.
  (doseq [actor [:user/a :user/b]
          version [-7 0 1 7 99 9223372036854775808N]
          note [::absent nil "" "ok"]]
    (let [b (cond-> (assoc pre :request/version version)
              (not= note ::absent) (assoc :request/note note))
          a (cond-> (assoc post
                           :request/owner actor
                           :request/version version)
              (not= note ::absent) (assoc :request/note note))
          x {:input/actor actor}
          ver (frame/audit-frame-witness! claim stable-ver b x a)
          nt (frame/audit-frame-witness! claim stable-note b x a)]
      (is (= :checked-conditional-frame-instance (:status ver)))
      (is (= :checked-conditional-frame-instance (:status nt)))
      (is (true? (:matches-transition? ver)))
      (is (true? (:reader-projections-equal? ver)))
      (is (true? (:reader-projections-equal? nt)))
      (is (= (>= version 0) (:before-invariant ver)))
      (is (= (>= version 0) (:after-invariant ver)))
      (is (= (or (= note ::absent) (and (some? note) (not= note "")))
             (:before-invariant nt)))
      (is (= (:before-invariant nt) (:after-invariant nt)))
      (is (= :not-issued (:certificate nt))))))

(deftest v1354-frame-changes-never-count-as-matching-transitions
  (doseq [bad-after [(assoc post :request/version 8)
                     (assoc post :request/note nil)
                     (assoc post :request/note "new")]]
    (let [r (frame/audit-frame-witness! claim stable-note pre inp bad-after)]
      (is (= :nonmatching-transition (:status r)))
      (is (false? (:matches-transition? r)))
      (is (= :not-universally-proved (:proof-status r)))
      (is (= :not-issued (:certificate r)))))
  (is (rejects? #(frame/audit-frame-witness!
                  claim stable-note pre inp (dissoc post :request/version))))
  (let [b (assoc pre :request/note nil)
        a post
        r (frame/audit-frame-witness! claim stable-note b inp a)]
    (is (= :nonmatching-transition (:status r)))
    (is (false? (:reader-projections-equal? r)))))

(deftest v1354-invalid-guard-never-proves-preservation
  (let [not-open (assoc pre :request/status :claimed)
        not-ownerless (assoc pre :request/owner :user/b)]
    (doseq [b [not-open not-ownerless]]
      (let [r (frame/audit-frame-witness! claim stable-ver b inp post)]
        (is (= :nonmatching-transition (:status r)))
        (is (false? (:matches-transition? r)))
        (is (= :not-issued (:certificate r)))))))

(deftest v1354-invariant-false-is-not-a-claim-of-initialization
  (let [b (assoc pre :request/version -10)
        a (assoc post :request/version -10)
        r (frame/audit-frame-witness! claim stable-ver b inp a)]
    (is (= :checked-conditional-frame-instance (:status r)))
    (is (false? (:before-invariant r)))
    (is (false? (:after-invariant r)))
    (is (true? (:preservation-implication-holds? r)))
    (is (= :not-universally-proved (:proof-status r)))))

(deftest v1354-reader-metadata-rejected
  (doseq [bad [(assoc stable-ver :readers (with-meta #{:request/version} {:trusted true}))
               (assoc stable-ver :expression (with-meta (:expression stable-ver) {:proof true}))
               (assoc stable-ver :expression [:>= (with-meta [:read :request/version] {:trust true}) [:literal 0]])
               (with-meta stable-ver {:claim :verified})]]
    (is (rejects? #(frame/derive-frame-lemma! claim bad)))))

(deftest v1354-ill-typed-predicates-not-admitted-by-footprint
  (doseq [expression [[:literal 12]
                      [:> [:read :request/version] [:literal nil]]
                      [:> [:read :request/version] [:literal "7"]]
                      [:and [:literal true] [:literal 1]]
                      [:mystery [:read :request/version] [:literal 7]]]]
    (let [bad {:id :example/bad-invariant
               :readers (if (re-find #"request/version" (pr-str expression))
                          #{:request/version} #{})
               :expression expression}]
      (is (rejects? #(frame/derive-frame-lemma! claim bad)) (pr-str expression)))))

(deftest v1354-unqualified-and-outside-state-readers-rejected
  (doseq [bad [{:id :bad :readers #{} :expression [:literal true]}
               {:id :example/invalid-reader
                :readers #{:not-qualified}
                :expression [:present [:read :not-qualified]]}
               {:id :example/unknown-field
                :readers #{:other/field}
                :expression [:present [:read :other/field]]}
               (assoc stable-ver :readers [:request/version])]]
    (is (rejects? #(frame/derive-frame-lemma! claim bad)))))

(deftest v1354-unexpected-invariant-keys-rejected
  (doseq [bad [(assoc stable-ver :approved true)
               (dissoc stable-ver :id)
               (dissoc stable-ver :expression)
               (dissoc stable-ver :readers)
               (into (sorted-map) stable-ver)
               []
               nil]]
    (is (rejects? #(frame/derive-frame-lemma! claim bad)))))

(deftest v1354-source-footprint-and-assignments-not-forgeable
  (doseq [bad [(assoc claim :writes #{:request/owner})
               (assoc claim :reads #{:request/status})
               (assoc claim :assignments {:request/owner [:read :input/actor]})
               (assoc-in claim [:assignments :request/owner] [:literal :user/b])
               (assoc-in claim [:assignments :request/owner] [:fn identity])
               (assoc claim :state-types (assoc (:state-types claim) :extra/field {:type :integer}))]]
    ;; Source changes must either fail admission or produce a different source
    ;; identity. In particular a type/field addition may be a valid declaration.
    (let [result (try {:accepted (frame/derive-frame-lemma! bad stable-ver)}
                      (catch clojure.lang.ExceptionInfo ex {:error ex}))]
      (if-let [lemma (:accepted result)]
        (is (not= (:transition-fingerprint
                    (frame/derive-frame-lemma! claim stable-ver))
                  (:transition-fingerprint lemma)))
        (is (instance? clojure.lang.ExceptionInfo (:error result)))))))

(deftest v1354-false-kernel-like-flags-do-not-grant-authority
  (let [proof (frame/derive-frame-lemma! claim stable-ver)
        witness (frame/audit-frame-witness! claim stable-ver pre inp post)]
    (doseq [r [proof witness]]
      (is (= :not-issued (:certificate r)))
      (is (= :not-admitted (:admission r)))
      (is (= :none (:authority r)))
      (is (not= :proved (:proof-status r))))
    ;; Reports are ordinary EDN, not opaque attestations.
    (is (= proof (edn/read-string (pr-str proof))))
    (is (= witness (edn/read-string (pr-str witness))))))

(deftest v1354-forged-transition-observation-is-challenged
  ;; A substituted observer saying "matched" cannot turn a modified frame
  ;; into the conditional theorem's valid premise.
  (with-redefs [independent/observe-transition! (fn [& _] {:matches? true})]
    (is (= :independent-frame-extensionality-disagreement
           (exception-reason
            #(frame/audit-frame-witness! claim stable-ver pre inp
                                         (assoc post :request/version 17)))))))

(deftest v1354-syntactic-limit-rejection
  (let [too-deep (nth (iterate (fn [e] [:not e]) [:literal true]) 70)
        too-wide (into [:and] (repeat 260 [:literal true]))]
    (doseq [e [too-deep too-wide]]
      (is (rejects? #(frame/derive-frame-lemma!
                      claim {:id :example/oversized
                             :readers #{} :expression e}))))))

(deftest v1354-source-identities-follow-semantic-changes
  (let [a (frame/derive-frame-lemma! claim stable-ver)
        b (frame/derive-frame-lemma!
           (assoc-in claim [:assignments :request/status] [:literal :open]) stable-ver)
        c (frame/derive-frame-lemma!
           claim (assoc stable-ver :expression
                        [:>= [:read :request/version] [:literal 7]]))]
    (is (not= (:transition-fingerprint a) (:transition-fingerprint b)))
    (is (not= (:typed-invariant-fingerprint a) (:typed-invariant-fingerprint c)))
    (is (= (:transition-fingerprint a) (:transition-fingerprint c)))
    (is (= (:typed-invariant-fingerprint a) (:typed-invariant-fingerprint b)))
    (is (= (:source-invariant c)
           (assoc stable-ver :expression [:>= [:read :request/version] [:literal 7]])))))

(deftest v1354-constant-false-invariant-is-conditionally-preserved
  (let [invariant {:id :example/always-false
                   :readers #{} :expression [:literal false]}
        l (frame/derive-frame-lemma! claim invariant)
        w (frame/audit-frame-witness! claim invariant pre inp post)]
    (is (= :conditional-frame-rule-instance (:status l)))
    (is (= :checked-conditional-frame-instance (:status w)))
    (is (false? (:before-invariant w)))
    (is (false? (:after-invariant w)))
    (is (true? (:preservation-implication-holds? w)))
    (is (= :not-issued (:certificate w)))))

(deftest v1354-projections-compare-presence-not-truthiness
  (doseq [[note-b note-a expected]
          [[nil nil true]
           [false false :invalid]  ; field type is nullable string, not boolean
           [::absent nil false]
           [nil ::absent false]]]
    (let [b (cond-> pre (not= note-b ::absent) (assoc :request/note note-b))
          a (cond-> post (not= note-a ::absent) (assoc :request/note note-a))]
      (if (= :invalid expected)
        (is (rejects? #(frame/audit-frame-witness! claim stable-note b inp a)))
        (let [r (frame/audit-frame-witness! claim stable-note b inp a)]
          (is (= expected (:reader-projections-equal? r)))
          (is (= (if expected :checked-conditional-frame-instance :nonmatching-transition)
                 (:status r))))))))

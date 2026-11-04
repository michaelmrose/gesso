(ns gesso.assurance.proof.transition-test
  "v1234: permanent adversarial regression coverage for v1233's CLOSED,
   PURELY ABSTRACT guarded-assignment relation. Tests source declarations,
   before/input/after projections, frame presence, nominal types, exact
   footprints, fingerprint freshness, and deliberately non-authoritative
   observations. No production commit or mathematical preservation theorem
   is claimed or exercised by this test namespace."
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.assurance.proof.transition :as transition]))

(def ^:private base-subject
  {:kind :operation :id :example/claim
   :interface {:operation :example/claim}})

(def ^:private claim
  {:subject base-subject
   :state-types
   {:request/status {:type :enum :name :request/status :values #{:open :claimed}}
    :request/owner {:type :keyword :nullable? true}
    :request/version {:type :integer}
    :request/note {:type :string :optional? true :nullable? true}}
   :input-types {:input/actor {:type :keyword}}
   :guard {:readers #{:request/status :request/owner}
           :expression [:and
                        [:= [:read :request/status] [:literal :open]]
                        [:= [:read :request/owner] [:literal nil]]]}
   :reads #{:request/status :request/owner :input/actor}
   :writes #{:request/status :request/owner}
   :assignments {:request/status [:literal :claimed]
                 :request/owner [:read :input/actor]}})

(def ^:private before
  {:request/status :open :request/owner nil :request/version 17})
(def ^:private inputs {:input/actor :user/a})
(def ^:private after
  {:request/status :claimed :request/owner :user/a :request/version 17})

(defn- observe
  ([pre in post] (transition/observe-transition! claim pre in post))
  ([spec pre in post] (transition/observe-transition! spec pre in post)))

(defn- error-data [thunk]
  (try (thunk) nil
       (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn- rejects! [thunk]
  (let [error (error-data thunk)]
    (is (map? error) "Expected a typed rejection, not acceptance")
    (is (qualified-keyword? (:error/type error)) (pr-str error))
    (is (some? (:at error)) (pr-str error))
    error))

(defn- reject-declaration! [spec]
  (rejects! #(transition/describe-transition! spec)))

(defn- reject-observation! [spec pre in post]
  (rejects! #(observe spec pre in post)))

(defn- cannot-grant-authority! [report]
  (is (= :not-proved (:proof-status report)))
  (is (= :not-admitted (:admission report)))
  (is (= :not-established (:runtime-enforcement report)))
  (is (= :none (:authority report)))
  (is (not (contains? report :certificate)))
  (is (not (contains? report :valid?)))
  (is (not (contains? report :transaction-authority)))
  report)

(deftest source-identity-is-versioned-and-unadmitted
  (let [description (transition/describe-transition! claim)]
    (is (= :gesso.assurance.proof.transition/unadmitted-transition
           (:type description)))
    (is (= 1 (:version description)))
    (is (= :syntax-and-types (:checking description)))
    (is (= :guarded-assignments-and-frame (:interpretation description)))
    (is (= (:version description) (get-in description [:semantics :version])))
    (is (= #{:request/note :request/version}
           (get-in description [:semantics :frame-fields])))
    (is (= #{:request/status :request/owner}
           (get-in description [:semantics :writes])))
    (is (= #{:request/status :request/owner :input/actor}
           (get-in description [:semantics :reads])))
    (is (re-matches #"sha256:[0-9a-f]{64}" (:fingerprint description)))
    (is (re-matches #"sha256:[0-9a-f]{64}" (:subject-fingerprint description)))
    (is (not= (:fingerprint description) (:subject-fingerprint description)))
    (is (= description (transition/describe-transition! claim)))
    (cannot-grant-authority! description)))

(deftest admitted-abstract-relation-is-not-transaction-authority
  (let [description (transition/describe-transition! claim)
        report (observe before inputs after)]
    (is (= :gesso.assurance.proof.transition/abstract-observation
           (:type report)))
    (is (= 1 (:version report)))
    (is (= :abstract-relation-observed (:checking report)))
    (is (= (:fingerprint description) (:transition-fingerprint report)))
    (is (= (:subject-fingerprint description) (:subject-fingerprint report)))
    (is (true? (:guard-holds? report)))
    (is (true? (:assignments-hold? report)))
    (is (true? (:frame-holds? report)))
    (is (true? (:matches? report)))
    (is (= report (observe before inputs after)))
    (cannot-grant-authority! report)))

(deftest guard-is-evaluated-against-prestate-not-poststate
  (let [r (observe (assoc before :request/status :claimed) inputs after)]
    (is (false? (:guard-holds? r)))
    (is (true? (:assignments-hold? r)))
    (is (true? (:frame-holds? r)))
    (is (false? (:matches? r))))
  (let [r (observe (assoc before :request/owner :user/b) inputs after)]
    (is (false? (:guard-holds? r)))
    (is (true? (:assignments-hold? r)))
    (is (true? (:frame-holds? r)))
    (is (false? (:matches? r))))
  (let [r (observe before inputs (assoc after :request/status :open))]
    (is (true? (:guard-holds? r)))
    (is (false? (:assignments-hold? r)))
    (is (true? (:frame-holds? r)))
    (is (false? (:matches? r)))))

(deftest assignments-must-match-the-actual-input
  (doseq [actor [:user/a :user/b :user/c]]
    (let [input {:input/actor actor}
          correct (assoc after :request/owner actor)
          wrong (assoc after :request/owner :user/d)]
      (is (true? (:matches? (observe before input correct))))
      (is (false? (:matches? (observe before input wrong))))
      (is (false? (:assignments-hold? (observe before input wrong))))))
  (let [r (observe before inputs (assoc after :request/owner nil))]
    (is (true? (:guard-holds? r)))
    (is (false? (:assignments-hold? r)))
    (is (true? (:frame-holds? r)))))

(deftest frame-preserves-every-unchanged-required-field
  (doseq [value [0 1 16 18 5000N 100000000000000000000N -100000000000000000000N]]
    (let [r (observe before inputs (assoc after :request/version value))]
      (is (= (= value 17) (:frame-holds? r)))
      (is (= (= value 17) (:matches? r)))
      (is (true? (:assignments-hold? r)))))
  (is (true? (:matches? (observe
                         (assoc before :request/version 100000000000000000000N)
                         inputs
                         (assoc after :request/version 100000000000000000000N))))))

(deftest optional-field-presence-is-part-of-frame
  (let [cases [[before after true]
               [(assoc before :request/note "hello")
                (assoc after :request/note "hello") true]
               [(assoc before :request/note nil)
                (assoc after :request/note nil) true]
               [before (assoc after :request/note nil) false]
               [before (assoc after :request/note "hello") false]
               [(assoc before :request/note nil) after false]
               [(assoc before :request/note "hello") after false]
               [(assoc before :request/note "hello")
                (assoc after :request/note "changed") false]
               [(assoc before :request/note nil)
                (assoc after :request/note "hello") false]
               [(assoc before :request/note "hello")
                (assoc after :request/note nil) false]]]
    (doseq [[pre post expected] cases]
      (let [r (observe pre inputs post)]
        (is (= expected (:frame-holds? r)))
        (is (= expected (:matches? r)))
        (is (true? (:assignments-hold? r)))))))

(deftest no-op-is-representable-but-must-preserve-the-entire-frame
  (let [spec (-> claim
                 (assoc :input-types {}
                        :reads #{:request/status :request/owner}
                        :writes #{}
                        :assignments {}))]
    (is (= (set (keys (:state-types spec)))
           (get-in (transition/describe-transition! spec)
                   [:semantics :frame-fields])))
    (is (true? (:matches? (observe spec before {} before))))
    (is (false? (:matches? (observe spec before {} after))))
    (is (false? (:frame-holds? (observe spec before {} after))))
    (let [bad-pre (assoc before :request/status :claimed)
          r (observe spec bad-pre {} bad-pre)]
      (is (false? (:guard-holds? r)))
      (is (true? (:assignments-hold? r)))
      (is (true? (:frame-holds? r)))
      (is (false? (:matches? r))))))

(deftest same-value-written-fields-still-count-as-assigned
  (let [spec (-> claim
                 (assoc :input-types {}
                        :reads #{:request/status :request/owner}
                        :writes #{:request/owner}
                        :assignments {:request/owner [:read :request/owner]}))
        report (observe spec before {} before)]
    (is (= #{:request/owner} (get-in
                              (transition/describe-transition! spec)
                              [:semantics :writes])))
    (is (true? (:matches? report)))
    (is (false? (:matches? (observe spec before {} after))))))

(deftest nullable-target-can-be-cleared-using-a-typed-nil-literal
  (let [spec (-> claim
                 (assoc :input-types {}
                        :reads #{:request/status :request/owner}
                        :assignments {:request/status [:literal :claimed]
                                      :request/owner [:literal nil]}))]
    (is (true? (:matches? (observe spec before {}
                                  (assoc after :request/owner nil)))))
    (is (false? (:matches? (observe spec before {} after))))
    (cannot-grant-authority! (observe spec before {}
                                     (assoc after :request/owner nil)))))

(deftest all-three-runtime-maps-must-have-exact-declared-shapes
  (doseq [[pre input post]
          [[(assoc before :extra/field 1) inputs after]
           [before (assoc inputs :extra/field 1) after]
           [before inputs (assoc after :extra/field 1)]
           [(dissoc before :request/version) inputs after]
           [before {} after]
           [before inputs (dissoc after :request/version)]
           [(assoc before :request/status :invalid) inputs after]
           [before inputs (assoc after :request/status :invalid)]
           [(assoc before :request/status "open") inputs after]
           [before {:input/actor "user/a"} after]
           [before inputs (assoc after :request/version 17.5)]
           [before inputs (assoc after :request/version "17")]
           [before inputs (assoc after :request/owner 12)]
           [before inputs (assoc after :request/note 12)]
           [before inputs (assoc after :request/note (fn [] :bad))]
           [before inputs (assoc after :request/owner (Object.))]
           [before inputs (assoc after :request/version nil)]
           [(assoc before :request/note (fn [] :bad)) inputs after]
           [before {:input/actor (fn [] :bad)} after]]]
    (reject-observation! claim pre input post)))

(deftest runtime-metadata-and-nonplain-maps-fail-closed
  (doseq [[pre input post]
          [[(with-meta before {:forged true}) inputs after]
           [before (with-meta inputs {:forged true}) after]
           [before inputs (with-meta after {:forged true})]
           [(sorted-map :request/status :open
                        :request/owner nil :request/version 17) inputs after]
           [before (sorted-map :input/actor :user/a) after]
           [before inputs (sorted-map :request/status :claimed
                                     :request/owner :user/a
                                     :request/version 17)]]]
    (reject-observation! claim pre input post)))

(deftest declaration-must-be-closed-and-unforgeable
  (doseq [spec [(assoc claim :valid? true)
                (assoc claim :proof-status :proved)
                (assoc claim :certificate {:valid? true})
                (assoc claim :admission :admitted)
                (assoc claim :runtime-enforcement :proven)
                (with-meta claim {:proof :yes})
                (dissoc claim :guard)
                (dissoc claim :assignments)
                (assoc claim :subject (fn [] nil))
                (assoc-in claim [:assignments :request/owner]
                          [:literal (fn [] :evil)])
                (assoc-in claim [:assignments :request/owner]
                          [:read (fn [] :evil)])]]
    (reject-declaration! spec)))

(deftest read-and-write-footprints-must-be-exact
  (doseq [spec [(assoc claim :reads #{:request/status :request/owner})
                (assoc claim :reads (conj (:reads claim) :request/version))
                (assoc claim :reads #{:request/status :request/owner :input/nope})
                (assoc claim :writes #{:request/status})
                (assoc claim :writes (conj (:writes claim) :request/version))
                (assoc claim :writes #{:request/other})
                (assoc claim :assignments {:request/status [:literal :claimed]})
                (assoc-in claim [:assignments :request/other] [:literal :claimed])
                (assoc claim :reads [:request/status :request/owner :input/actor])
                (assoc claim :writes [:request/status :request/owner])]]
    (reject-declaration! spec)))

(deftest unknown-overlapping-and-unused-declared-fields-fail-closed
  (doseq [spec [(assoc claim :state-types {})
                (assoc-in claim [:state-types :input/actor] {:type :keyword})
                (assoc-in claim [:input-types :request/status] {:type :keyword})
                (assoc-in claim [:state-types :gesso.assurance.proof.transition/target]
                          {:type :integer})
                (assoc-in claim [:input-types :input/unused] {:type :string})
                (assoc-in claim [:guard :readers] #{:request/status :request/unknown})
                (assoc-in claim [:guard :expression]
                          [:= [:read :request/status] [:read :request/unknown]])
                (assoc claim :reads #{:request/status :request/owner :input/actor
                                      :request/missing})
                (assoc-in claim [:state-types :plain] {:type :keyword})]]
    (reject-declaration! spec)))

(deftest malformed-ill-typed-or-opaque-assignments-fail-closed
  (doseq [expression [[:literal :not-in-status-enum]
                      [:literal "claimed"]
                      [:literal nil]
                      [:literal 3]
                      [:read :input/actor]
                      [:read :request/unknown]
                      [:call :opaque/computation]
                      [:literal (fn [] :claimed)]
                      [:read :request/note]
                      [:set :claimed]
                      [:literal]
                      [:literal :claimed :extra]
                      [:= [:read :request/status] [:literal :claimed]]
                      {:literal :claimed}]]
    (reject-declaration! (assoc-in claim [:assignments :request/status] expression)))
  (reject-declaration!
   (assoc-in claim [:assignments :request/owner] [:read :request/note])))

(deftest optional-and-nullable-assignment-readers-are-restricted
  (let [optional-source (assoc-in claim [:assignments :request/owner]
                                  [:read :request/note])]
    (reject-declaration! optional-source))
  (let [nullable-source (-> claim
                            (assoc-in [:assignments :request/owner]
                                      [:read :request/owner])
                            (assoc :input-types {}
                                   :reads #{:request/status :request/owner}))]
    (is (map? (transition/describe-transition! nullable-source))))
  (let [nonnullable-target
        (assoc-in claim [:state-types :request/owner] {:type :keyword})]
    (reject-declaration! nonnullable-target)))

(deftest nonboolean-guards-and-unsafe-reads-rejected-before-observation
  (doseq [guard [{:readers #{:request/status}
                 :expression [:read :request/status]}
                {:readers #{:request/version}
                 :expression [:> [:read :request/version] [:literal "4"]]}
                {:readers #{:request/status}
                 :expression [:and [:literal true]
                              [:> [:read :request/status] [:literal 1]]]}
                {:readers #{:request/status}
                 :expression [:unknown [:read :request/status]]}]]
    (reject-declaration!
     (assoc claim :guard guard))))

(deftest identity-changes-for-meaningful-semantic-differences
  (let [id (:fingerprint (transition/describe-transition! claim))
        variations [(assoc-in claim [:subject :id] :example/claim-v2)
                    (assoc-in claim [:subject :interface :operation]
                              :example/other)
                    (assoc-in claim [:guard :expression 1 0] :not=)
                    (assoc-in claim [:guard :expression 2 0] :not=)
                    (assoc-in claim [:assignments :request/status]
                              [:literal :open])
                    (-> claim
                        (assoc-in [:assignments :request/owner]
                                  [:literal :user/a])
                        (assoc :input-types {}
                               :reads #{:request/status :request/owner}))
                    (assoc-in claim [:state-types :request/note :nullable?] false)
                    (assoc-in claim [:state-types :request/version :type] :string)
                    (assoc-in claim [:state-types :request/status :values]
                              #{:open :claimed :closed})]]
    (doseq [variant variations]
      (is (not= id (:fingerprint (transition/describe-transition! variant)))))
    (is (= id (:fingerprint
               (transition/describe-transition!
                (assoc-in claim [:state-types :request/version :optional?] false)))))
    (is (= id (:fingerprint
               (transition/describe-transition!
                (assoc-in claim [:state-types :request/note :nullable?] true)))))))

(deftest subject-identity-is-independent-of-abstract-state-values
  (let [first-report (observe before inputs after)
        second-report (observe (assoc before :request/version 18)
                               inputs (assoc after :request/version 18))]
    (is (:matches? first-report))
    (is (:matches? second-report))
    (is (= (:transition-fingerprint first-report)
           (:transition-fingerprint second-report)))
    (is (= (:subject-fingerprint first-report)
           (:subject-fingerprint second-report)))))

(deftest type-resource-limits-reject-unbounded-projections
  (let [many-fields (into {} (map (fn [i] [(keyword "request" (str "f" i))
                                         {:type :integer}]) (range 130)))]
    (reject-declaration! (assoc claim :state-types many-fields)))
  (let [many-fields (into {} (map (fn [i] [(keyword "input" (str "x" i))
                                         {:type :integer}]) (range 130)))]
    (reject-declaration! (assoc claim :input-types many-fields))))

(deftest deterministic-exhaustive-small-state-space-relational-oracle
  ;; Independent, finite oracle: for this fixed declaration, the only allowed
  ;; transition is open/unowned -> claimed/actor, with version and note fixed.
  ;; This is finite TEST COVERAGE, not a universal proof of implementation.
  (let [prestates [{:request/status :open :request/owner nil :request/version 0}
                   {:request/status :open :request/owner :user/a :request/version 0}
                   {:request/status :claimed :request/owner nil :request/version 1}
                   {:request/status :claimed :request/owner :user/b
                    :request/version 1 :request/note "memo"}]
        inputs [{:input/actor :user/a} {:input/actor :user/b}]
        poststates (for [status [:open :claimed]
                         owner [nil :user/a :user/b]
                         version [0 1]
                         note [::missing "memo"]]
                     (cond-> {:request/status status
                              :request/owner owner
                              :request/version version}
                       (not= note ::missing) (assoc :request/note note)))]
    (doseq [pre prestates in inputs post poststates]
      (let [expected (and (= :open (:request/status pre))
                          (nil? (:request/owner pre))
                          (= :claimed (:request/status post))
                          (= (:input/actor in) (:request/owner post))
                          (= (:request/version pre) (:request/version post))
                          (= (contains? pre :request/note)
                             (contains? post :request/note))
                          (= (:request/note pre) (:request/note post)))
            report (observe pre in post)]
        (is (= expected (:matches? report))
            (str "pre=" (pr-str pre) " in=" (pr-str in) " post=" (pr-str post)))))))

(deftest integer-frame-mutation-matrix
  (let [seed 100000000000000000000000000000000N]
    (doseq [n (range 90)]
      (let [n (+ seed n)
            pre (assoc before :request/version n)
            post (assoc after :request/version n)
            changed (assoc after :request/version (inc n))]
        (is (:matches? (observe pre inputs post)))
        (is (false? (:frame-holds? (observe pre inputs changed))))
        (is (false? (:matches? (observe pre inputs changed))))))))

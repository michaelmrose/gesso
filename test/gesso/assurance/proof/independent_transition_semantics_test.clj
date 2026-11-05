(ns gesso.assurance.proof.independent-transition-semantics-test
  (:require [clojure.test :refer [deftest is testing]]
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

(defn- observe [source before inputs after]
  (independent/observe-transition! source before inputs after))

(defn- reject? [thunk]
  (try (thunk) false (catch clojure.lang.ExceptionInfo _ true)))

(deftest matches-prod-claim
  (let [actual (observe claim pre inp post)
        prod (production/observe-transition! claim pre inp post)]
    (is (= (select-keys prod [:guard-holds? :assignments-hold?
                             :frame-holds? :matches?])
           (select-keys actual [:guard-holds? :assignments-hold?
                                :frame-holds? :matches?])))
    (is (true? (:matches? actual)))
    (is (= #{:request/note :request/version} (:frame-fields actual)))
    (is (= :none (:authority actual)))
    (is (= :not-proved (:proof-status actual)))))

(deftest guard-prestate-not-poststate
  (doseq [[p i a] [[(assoc pre :request/status :claimed) inp post]
                    [(assoc pre :request/owner :user/b) inp post]
                    [pre inp (assoc post :request/status :open)]]]
    (is (= (select-keys (production/observe-transition! claim p i a)
                        [:guard-holds? :assignments-hold? :frame-holds? :matches?])
           (select-keys (observe claim p i a)
                        [:guard-holds? :assignments-hold? :frame-holds? :matches?])))))

(deftest assignments-reference-inputs-and-prestate
  (doseq [actor [:user/a :user/b :user/c]]
    (let [i {:input/actor actor}
          a (assoc post :request/owner actor)]
      (is (true? (:matches? (observe claim pre i a))))
      (is (= (= actor :user/a) (:matches? (observe claim pre i post))))
      (is (= (:matches? (production/observe-transition! claim pre i a))
             (:matches? (observe claim pre i a))))))
  (let [s (-> claim
              (assoc :input-types {}
                     :reads #{:request/status :request/owner}
                     :writes #{:request/owner}
                     :assignments {:request/owner [:read :request/owner]}))]
    (is (true? (:matches? (observe s pre {} pre))))
    (is (false? (:matches? (observe s pre {} post))))))

(deftest frame-observes-presence-and-value
  (doseq [[p a expected]
          [[pre post true]
           [(assoc pre :request/note nil) (assoc post :request/note nil) true]
           [(assoc pre :request/note "hello")
            (assoc post :request/note "hello") true]
           [pre (assoc post :request/note nil) false]
           [(assoc pre :request/note nil) post false]
           [(assoc pre :request/note "hello")
            (assoc post :request/note "changed") false]
           [pre (assoc post :request/version 8) false]]]
    (is (= expected (:frame-holds? (observe claim p inp a))))))

(deftest literal-assignment-and-optional-target
  (let [s (-> claim
              (assoc :input-types {}
                     :reads #{:request/status :request/owner}
                     :writes #{:request/owner :request/note}
                     :assignments {:request/owner [:literal nil]
                                   :request/note [:literal "saved"]}))
        a (assoc pre :request/note "saved")]
    (is (true? (:matches? (observe s pre {} a))))
    (is (false? (:matches? (observe s pre {} (dissoc a :request/note)))))
    (is (= (:matches? (production/observe-transition! s pre {} a))
           (:matches? (observe s pre {} a))))))

(deftest invalid-snapshots-reject
  (doseq [[p i a] [[(dissoc pre :request/version) inp post]
                    [pre {} post]
                    [pre inp (assoc post :extra/s 1)]
                    [pre inp (assoc post :request/version nil)]
                    [pre inp (assoc post :request/status :bad)]
                    [pre (assoc inp :extra/x false) post]]]
    (is (reject? #(observe claim p i a)))))

(deftest malformed-declarations-reject
  (doseq [bad [(assoc claim :extra/x true)
               (assoc claim :reads #{:request/status})
               (assoc claim :writes #{:request/status})
               (assoc claim :assignments {:request/status [:literal :claimed]
                                          :request/owner [:read :input/actor]
                                          :request/note [:literal "x"]})
               (assoc-in claim [:assignments :request/owner] [:fn inc])
               (assoc-in claim [:assignments :request/owner] [:read :extra/x])
               (assoc-in claim [:state-types :request/owner :nullable?] false)
               (assoc-in claim [:subject :interface :operation] (fn [] 1))
               (assoc claim :input-types {:request/status {:type :keyword}})
               (assoc claim :guard {:readers #{} :expression [:literal true]})]]
    (is (reject? #(observe bad pre inp post)) (pr-str (dissoc bad :subject)))))

(deftest input-reader-never-consults-poststate
  (let [s (-> claim
              (assoc :guard {:readers #{:request/status :input/actor}
                             :expression [:and [:= [:read :request/status] [:literal :open]]
                                          [:= [:read :input/actor] [:literal :user/a]]]})
              (assoc :reads #{:request/status :input/actor}))]
    (is (true? (:matches? (observe s pre inp post))))
    (is (false? (:matches? (observe s pre {:input/actor :user/b}
                                   (assoc post :request/owner :user/b)))))))

(deftest empty-guard-and-noop
  (let [s (-> claim
              (assoc :input-types {}
                     :reads #{}
                     :writes #{}
                     :assignments {}
                     :guard {:readers #{} :expression [:literal true]}))]
    (is (true? (:matches? (observe s pre {} pre))))
    (is (false? (:matches? (observe s pre {} post))))))

(deftest protected-against-metadata-and-source-injection
  (is (reject? #(observe (with-meta claim {:evil true}) pre inp post)))
  (is (reject? #(observe (assoc-in claim [:assignments :request/owner]
                                    (with-meta [:read :input/actor] {:evil true}))
                        pre inp post)))
  (is (reject? #(observe (assoc-in claim [:subject :interface] {:evil/execute (fn [] true)})
                        pre inp post))))

(deftest finite-mutation-grid-matches-production
  (let [versions [0 1 7 8 999N]
        owners [nil :user/a :user/b]
        statuses [:open :claimed]
        notes [::absent nil "memo"]
        assemble (fn [status owner ver note]
                   (cond-> {:request/status status
                            :request/owner owner
                            :request/version ver}
                     (not= note ::absent) (assoc :request/note note)))
        post-states (for [status statuses owner owners version versions note notes]
                      (assemble status owner version note))]
    (doseq [before [(assemble :open nil 7 ::absent)
                    (assemble :open nil 7 "memo")
                    (assemble :claimed :user/b 7 nil)]
            input [{:input/actor :user/a} {:input/actor :user/b}]
            after post-states]
      (let [p (production/observe-transition! claim before input after)
            i (observe claim before input after)]
        (is (= (select-keys p [:guard-holds? :assignments-hold?
                               :frame-holds? :matches?])
               (select-keys i [:guard-holds? :assignments-hold?
                               :frame-holds? :matches?])))))))

(deftest assignments-must-read-old-state-even-if-target-is-written
  (let [spec (-> claim
                 (assoc :input-types {}
                        :reads #{:request/status :request/owner}
                        :writes #{:request/status :request/owner}
                        :assignments {:request/status [:literal :claimed]
                                      :request/owner [:read :request/owner]}))
        expected (assoc pre :request/status :claimed)]
    (is (true? (:matches? (observe spec pre {} expected))))
    (is (false? (:matches? (observe spec pre {} post))))
    (is (= (:matches? (production/observe-transition! spec pre {} expected))
           (:matches? (observe spec pre {} expected))))))

;; v1352 permanent correspondence and negative-control coverage.  The expected
;; outcomes in the finite oracles below are calculated from the declaration,
;; not copied from either transition implementation.  Equality of two
;; implementations by itself is not a proof of the semantics.

(def ^:private relation-keys
  [:guard-holds? :assignments-hold? :frame-holds? :matches?])

(defn- check-correspondence!
  [spec before inputs after]
  (let [ind (observe spec before inputs after)
        prod (production/observe-transition! spec before inputs after)]
    (is (= (select-keys prod relation-keys)
           (select-keys ind relation-keys))
        (str "observation disagreement: "
             (pr-str [spec before inputs after])))
    (is (= (boolean (and (:guard-holds? ind)
                         (:assignments-hold? ind)
                         (:frame-holds? ind)))
           (:matches? ind)))
    (is (= :none (:authority ind)))
    (is (= :not-proved (:proof-status ind)))
    (is (= :not-admitted (:admission ind)))
    (is (= :not-established (:physical-observation ind)))
    ind))

(defn- check-both-reject!
  [spec before inputs after]
  (is (reject? #(observe spec before inputs after))
      (str "independent unexpectedly accepted: " (pr-str [spec before inputs after])))
  (is (reject? #(production/observe-transition! spec before inputs after))
      (str "production unexpectedly accepted: " (pr-str [spec before inputs after]))))

(deftest v1352-independent-finite-claim-oracle
  (let [states (for [status [:open :claimed]
                     owner [nil :user/a :user/b]
                     version [0 7 900000000000000000000N]
                     note [::absent nil "memo"]]
                 (cond-> {:request/status status
                          :request/owner owner
                          :request/version version}
                   (not= note ::absent) (assoc :request/note note)))
        outputs (for [status [:open :claimed]
                      owner [nil :user/a :user/b]
                      version [0 7]
                      note [::absent "memo"]]
                  (cond-> {:request/status status
                           :request/owner owner
                           :request/version version}
                    (not= note ::absent) (assoc :request/note note)))]
    ;; Independently calculate the relation, including exact optional presence.
    ;; A changing frame value is *not* a malformed observation; it is false.
    (doseq [before (take 12 states)
            input [{:input/actor :user/a} {:input/actor :user/b}]
            after outputs]
      (let [expected-guard (and (= :open (:request/status before))
                                (nil? (:request/owner before)))
            expected-assignments (and (= :claimed (:request/status after))
                                      (= (:input/actor input) (:request/owner after)))
            expected-frame (and (= (:request/version before) (:request/version after))
                                (= (contains? before :request/note)
                                   (contains? after :request/note))
                                (= (:request/note before) (:request/note after)))
            r (check-correspondence! claim before input after)]
        (is (= (boolean expected-guard) (:guard-holds? r)))
        (is (= (boolean expected-assignments) (:assignments-hold? r)))
        (is (= (boolean expected-frame) (:frame-holds? r)))
        (is (= (boolean (and expected-guard expected-assignments expected-frame))
               (:matches? r)))))))

(deftest v1352-guard-observes-prestate-even-when-poststate-satisfies-it
  (doseq [before [(assoc pre :request/status :claimed)
                  (assoc pre :request/owner :user/b)
                  (assoc pre :request/status :claimed :request/owner :user/b)]]
    (let [r (check-correspondence! claim before inp post)]
      (is (false? (:guard-holds? r)))
      (is (true? (:assignments-hold? r)))
      (is (false? (:matches? r))))))

(deftest v1352-written-target-cannot-be-absent
  (let [spec (-> claim
                 (assoc :input-types {}
                        :reads #{:request/status :request/owner}
                        :writes #{:request/note}
                        :assignments {:request/note [:literal "new"]}))
        after (assoc pre :request/note "new")]
    (is (:matches? (check-correspondence! spec pre {} after)))
    (let [r (check-correspondence! spec pre {} pre)]
      (is (false? (:assignments-hold? r)))
      (is (false? (:matches? r))))
    (let [r (check-correspondence! spec
                                   (assoc pre :request/note nil) {}
                                   after)]
      (is (true? (:matches? r))))))

(deftest v1352-optional-frame-must-retain-presence-exactly
  (doseq [[before after expected]
          [[pre post true]
           [(assoc pre :request/note nil) (assoc post :request/note nil) true]
           [(assoc pre :request/note false) (assoc post :request/note false) :invalid]
           [(assoc pre :request/note "memo") (assoc post :request/note "memo") true]
           [pre (assoc post :request/note nil) false]
           [(assoc pre :request/note nil) post false]
           [(assoc pre :request/note nil) (assoc post :request/note "memo") false]
           [(assoc pre :request/note "memo") (assoc post :request/note nil) false]]]
    (if (= expected :invalid)
      (check-both-reject! claim before inp after)
      (let [r (check-correspondence! claim before inp after)]
        (is (= expected (:frame-holds? r)))))))

(deftest v1352-simultaneous-swap-uses-old-values
  (let [spec {:subject {:kind :operation :id :example/swap
                        :interface {:operation :example/swap}}
              :state-types {:pair/left {:type :integer}
                            :pair/right {:type :integer}
                            :pair/label {:type :string :optional? true}}
              :input-types {}
              :guard {:readers #{} :expression [:literal true]}
              :reads #{:pair/left :pair/right}
              :writes #{:pair/left :pair/right}
              :assignments {:pair/left [:read :pair/right]
                            :pair/right [:read :pair/left]}}]
    (doseq [left [-7 0 7 100000000000000000000N]
            right [-3 0 3]
            label [::absent "kept"]]
      (let [before (cond-> {:pair/left left :pair/right right}
                     (not= label ::absent) (assoc :pair/label label))
            after (assoc before :pair/left right :pair/right left)]
        (is (true? (:matches? (check-correspondence! spec before {} after))))
        (when (not= left right)
          (is (false? (:matches? (check-correspondence! spec before {} before))))
          (is (false? (:matches? (check-correspondence! spec before {}
                                                       (assoc after :pair/right right))))))))))

(deftest v1352-noop-still-enforces-frame-and-guard
  (let [spec (-> claim
                 (assoc :input-types {}
                        :reads #{}
                        :writes #{}
                        :assignments {}
                        :guard {:readers #{} :expression [:literal true]}))]
    (is (:matches? (check-correspondence! spec pre {} pre)))
    (doseq [after [(assoc pre :request/version 8)
                   (assoc pre :request/note nil)
                   (assoc pre :request/owner :user/b)]]
      (let [r (check-correspondence! spec pre {} after)]
        (is (:guard-holds? r))
        (is (false? (:frame-holds? r)))
        (is (false? (:matches? r)))))))

(deftest v1352-false-guard-never-licenses-writes
  (let [spec (-> claim
                 (assoc :input-types {}
                        :reads #{}
                        :guard {:readers #{} :expression [:literal false]}
                        :writes #{:request/status}
                        :assignments {:request/status [:literal :claimed]}))
        good-write (assoc pre :request/status :claimed)]
    (is (= {:guard-holds? false :assignments-hold? true
            :frame-holds? true :matches? false}
           (select-keys (check-correspondence! spec pre {} good-write)
                        relation-keys)))
    (is (false? (:matches? (check-correspondence! spec pre {} pre))))))

(deftest v1352-prestate-copy-cannot-read-after-written-field
  (let [spec (-> claim
                 (assoc :input-types {}
                        :reads #{:request/status :request/owner}
                        :writes #{:request/status :request/owner}
                        :assignments {:request/status [:literal :claimed]
                                      :request/owner [:read :request/owner]}))
        expected (assoc pre :request/status :claimed)]
    (is (:matches? (check-correspondence! spec pre {} expected)))
    (is (false? (:matches? (check-correspondence! spec pre {} post))))))

(deftest v1352-guard-reads-input-before-evaluating-assignment
  (let [spec (-> claim
                 (assoc :guard {:readers #{:request/status :input/actor}
                                :expression [:and
                                             [:= [:read :request/status] [:literal :open]]
                                             [:= [:read :input/actor] [:literal :user/a]]]})
                 (assoc :reads #{:request/status :input/actor}))]
    (is (:matches? (check-correspondence! spec pre inp post)))
    (let [i {:input/actor :user/b}
          a (assoc post :request/owner :user/b)
          r (check-correspondence! spec pre i a)]
      (is (false? (:guard-holds? r)))
      (is (true? (:assignments-hold? r)))
      (is (false? (:matches? r))))))

(deftest v1352-guard-presence-short-circuits-safely
  (let [spec (-> claim
                 (assoc :guard {:readers #{:request/note}
                                :expression [:and
                                             [:present [:read :request/note]]
                                             [:not= [:read :request/note]
                                              [:literal nil]]]})
                 (assoc :reads #{:request/note :input/actor}))]
    (doseq [[before guard] [[pre false]
                            [(assoc pre :request/note nil) false]
                            [(assoc pre :request/note "memo") true]]]
      (let [after (cond-> post
                    (contains? before :request/note)
                    (assoc :request/note (:request/note before)))
            r (check-correspondence! spec before inp after)]
        (is (= guard (:guard-holds? r)))
        (is (= guard (:matches? r)))))))

(deftest v1352-assignment-literal-and-enum-typing-are-independent
  (let [spec (-> claim
                 (assoc :input-types {}
                        :reads #{:request/status :request/owner}
                        :assignments {:request/status [:literal :claimed]
                                      :request/owner [:literal nil]}))]
    (is (:matches? (check-correspondence! spec pre {}
                                         (assoc post :request/owner nil))))
    (is (false? (:matches? (check-correspondence! spec pre {} post))))
    (doseq [wrong [:unknown "claimed" nil 7]]
      (check-both-reject!
       (assoc-in spec [:assignments :request/status] [:literal wrong])
       pre {} post))))

(deftest v1352-declared-type-mismatches-are-not-false-transitions
  (doseq [[before input after]
          [[(assoc pre :request/status :not-declared) inp post]
           [(dissoc pre :request/version) inp post]
           [pre (assoc inp :input/actor 7) post]
           [pre {} post]
           [pre inp (assoc post :request/owner 7)]
           [pre inp (assoc post :request/status :not-declared)]
           [pre inp (assoc post :request/note false)]
           [pre inp (assoc post :request/version nil)]
           [pre inp (assoc post :unmapped/key false)]]]
    (check-both-reject! claim before input after)))

(deftest v1352-state-input-metadata-and-nonplain-maps-reject
  (doseq [[before input after]
          [[(with-meta pre {:proof true}) inp post]
           [pre (with-meta inp {:proof true}) post]
           [pre inp (with-meta post {:proof true})]
           [(sorted-map :request/status :open :request/owner nil
                        :request/version 7) inp post]
           [pre (sorted-map :input/actor :user/a) post]
           [pre inp (sorted-map :request/status :claimed :request/owner :user/a
                                 :request/version 7)]]]
    (check-both-reject! claim before input after)))

(deftest v1352-declaration-must-not-carry-forged-proof-markers
  (doseq [spec [(assoc claim :valid? true)
                (assoc claim :proof-status :proved)
                (assoc claim :admission :admitted)
                (assoc claim :certificate {:valid? true})
                (with-meta claim {:admitted? true})
                (assoc-in claim [:subject :interface :opaque] (fn [] true))
                (assoc-in claim [:assignments :request/owner]
                          (with-meta [:read :input/actor] {:proof :yes}))]]
    (check-both-reject! spec pre inp post)))

(deftest v1352-source-footprint-mutation-must-reject
  (doseq [spec [(assoc claim :reads #{:request/status :request/owner})
                (assoc claim :reads (conj (:reads claim) :request/version))
                (assoc claim :writes #{:request/status})
                (assoc claim :writes #{:request/status :request/owner :request/note})
                (assoc claim :assignments {:request/status [:literal :claimed]})
                (assoc-in claim [:assignments :request/owner] [:read :request/missing])
                (assoc-in claim [:assignments :request/owner] [:read :request/note])
                (assoc-in claim [:state-types :request/status :type] :boolean)
                (assoc-in claim [:input-types :input/actor :type] :integer)
                (assoc claim :input-types {:request/status {:type :keyword}})
                (assoc claim :guard {:readers #{:request/unknown}
                                    :expression [:present [:read :request/unknown]]})]]
    (check-both-reject! spec pre inp post)))

(deftest v1352-guard-source-cannot-have-illegal-reader-use
  (doseq [guard [{:readers #{:request/version}
                 :expression [:> [:read :request/version] [:literal "not-an-int"]]}
                {:readers #{:request/status}
                 :expression [:read :request/status]}
                {:readers #{:request/note}
                 :expression [:> [:read :request/note] [:literal "x"]]}
                {:readers #{:request/note}
                 :expression [:not= [:read :request/note] [:literal nil]]}]]
    (check-both-reject! (assoc claim :guard guard) pre inp post)))

(deftest v1352-source-size-and-capability-boundaries-remain-explicit
  (let [extra (into {} (for [i (range 61)]
                         [(keyword "request" (str "aux" i)) {:type :integer}]))
        source (-> claim
                   (update :state-types merge extra))
        before (merge pre (zipmap (keys extra) (repeat 0)))
        after (merge post (zipmap (keys extra) (repeat 0)))]
    ;; 4 original state fields + 1 input field + 61 = 66.  Independent
    ;; observer has a 64-reader ceiling; production is allowed 128.
    (is (reject? #(observe source before inp after)))
    (is (:matches? (production/observe-transition! source before inp after)))))

(deftest v1352-copyable-report-must-never-authorize-real-effects
  (let [r (check-correspondence! claim pre inp post)
        forged (assoc r :authority :committed :proof-status :proved
                      :admission :admitted :physical-observation :verified)]
    (is (:matches? r))
    (is (= (select-keys r relation-keys)
           (select-keys forged relation-keys)))
    (is (= :none (:authority (observe claim pre inp post))))
    (is (= :not-proved (:proof-status (observe claim pre inp post))))
    (is (= :not-established (:physical-observation
                            (observe claim pre inp post))))))

(ns gesso.assurance.proof.unified-preservation-test
  "v1268: source-derived tests for unified invariant preservation.

   All four sibling checkers must derive their own evidence from original source.
   An entire invariant must be checked by ONE sibling; apparent pieces of
   support supplied by different siblings cannot be combined into a theorem.
   Status :checked-conditional-inference is never a v1241 obligation discharge,
   soundness proof, application certificate, or physical execution authority."
  (:require [clojure.set :as set]
            [clojure.test :refer [deftest is testing]]
            [gesso.assurance.proof.assignment-preservation :as literal]
            [gesso.assurance.proof.copy-preservation :as copy]
            [gesso.assurance.proof.mixed-preservation :as mixed]
            [gesso.assurance.proof.obligation :as obligation]
            [gesso.assurance.proof.preservation :as frame]
            [gesso.assurance.proof.unified-preservation :as unified]
            [gesso.assurance.transition :as transition]))

(def ^:private operation
  {:subject {:kind :operation :id :example/unified-claim
             :interface {:operation :example/unified-claim}}
   :state-types
   {:request/status {:type :enum :name :request/status
                     :values #{:open :claimed :done}}
    :request/score {:type :integer}
    :request/original {:type :integer}
    :request/copied {:type :integer}
    :request/copied2 {:type :integer}
    :request/actor-one {:type :keyword}
    :request/actor-two {:type :keyword}
    :request/version {:type :integer}
    :request/note {:type :string :optional? true :nullable? true}}
   :input-types {:input/actor {:type :keyword}}
   :guard {:readers #{:request/status}
           :expression [:= [:read :request/status] [:literal :open]]}
   :reads #{:request/status :request/original :input/actor}
   :writes #{:request/status :request/score :request/copied :request/copied2
             :request/actor-one :request/actor-two}
   :replacements {:request/status [:literal :claimed]
                  :request/score [:literal 9]
                  :request/copied [:read :request/original]
                  :request/copied2 [:read :request/original]
                  :request/actor-one [:read :input/actor]
                  :request/actor-two [:read :input/actor]}
   :frame #{:request/original :request/version :request/note}})

(def ^:private rules
  {:request/frame-version
   {:readers #{:request/version}
    :expression [:>= [:read :request/version] [:literal 0]]}
   :request/frame-optional
   {:readers #{:request/note}
    :expression [:present [:read :request/note]]}
   :request/literal-status
   {:readers #{:request/status}
    :expression [:= [:read :request/status] [:literal :claimed]]}
   :request/literal-score
   {:readers #{:request/score}
    :expression [:> [:read :request/score] [:literal 7]]}
   :request/mixed-version-status
   {:readers #{:request/version :request/status}
    :expression [:and
                 [:>= [:read :request/version] [:literal 0]]
                 [:= [:read :request/status] [:literal :claimed]]]}
   :request/copy-source
   {:readers #{:request/copied :request/original}
    :expression [:= [:read :request/copied] [:read :request/original]]}
   :request/copy-pair
   {:readers #{:request/copied :request/copied2}
    :expression [:= [:read :request/copied] [:read :request/copied2]]}
   :request/copy-actors
   {:readers #{:request/actor-one :request/actor-two}
    :expression [:= [:read :request/actor-one] [:read :request/actor-two]]}
   :request/copy-with-frame
   {:readers #{:request/copied :request/original :request/version}
    :expression [:and
                 [:= [:read :request/copied] [:read :request/original]]
                 [:>= [:read :request/version] [:literal 0]]]}
   :request/copy-and-literal-splice
   {:readers #{:request/copied :request/original :request/status}
    :expression [:and
                 [:= [:read :request/copied] [:read :request/original]]
                 [:= [:read :request/status] [:literal :claimed]]]}
   :request/copy-and-literal-reversed
   {:readers #{:request/copied :request/original :request/status}
    :expression [:and
                 [:= [:read :request/status] [:literal :claimed]]
                 [:= [:read :request/copied] [:read :request/original]]]}
   :request/unsupported-origin
   {:readers #{:request/copied :request/version}
    :expression [:= [:read :request/copied] [:read :request/version]]}
   :request/unsupported-arithmetic
   {:readers #{:request/copied :request/original}
    :expression [:> [:read :request/copied] [:read :request/original]]}
   :request/false-literal
   {:readers #{:request/status}
    :expression [:= [:read :request/status] [:literal :open]]}
   :request/optional-dependent
   {:readers #{:request/note :request/status}
    :expression [:and
                 [:present [:read :request/note]]
                 [:= [:read :request/note] [:literal nil]]
                 [:= [:read :request/status] [:literal :claimed]]]}})

(def ^:private baseline
  {:construction operation
   :initial-condition {:readers #{:request/status}
                       :expression [:= [:read :request/status] [:literal :open]]}
   :invariants rules})

(def ^:private expected-checked
  #{:request/frame-version :request/frame-optional
    :request/literal-status :request/literal-score
    :request/mixed-version-status :request/copy-source :request/copy-pair
    :request/copy-actors :request/copy-with-frame})
(def ^:private expected-open
  (set/difference (set (keys rules)) expected-checked))

(def ^:private unified-error
  :gesso.assurance.proof.unified-preservation/invalid-unified-preservation)

(defn- check! [source]
  (unified/check-unified-preservation! source))
(defn- outcome [report invariant]
  (first (filter #(= invariant (:invariant %))
                 (:preservation-outcomes report))))
(defn- preservation-ids [ids]
  (set (map (fn [id] [:preservation id]) ids)))
(defn- rejection [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))
(defn- rejects! [f error-type reason]
  (let [d (rejection f)]
    (is (map? d) (str "Expected typed rejection " error-type "/" reason))
    (is (= error-type (:error/type d)) (pr-str d))
    (is (= reason (:reason d)) (pr-str d))
    d))
(defn- invariant-source [id]
  (assoc baseline :invariants (select-keys rules [id])))
(defn- change-invariant [id f]
  (update-in baseline [:invariants id] f))

(defn- assert-unadmitted! [source report]
  (let [ledger (obligation/derive-obligations! source)
        ids (preservation-ids (keys (:invariants source)))
        outcomes (:preservation-outcomes report)
        checked (:conditionally-checked-preservation-ids report)
        unsupported (:unsupported-preservation-ids report)]
    (is (= unified/report-type (:type report)))
    (is (= unified/unified-preservation-version (:version report)))
    (is (re-matches #"sha256:[0-9a-f]{64}" (:fingerprint report)))
    (is (= (:fingerprint ledger) (:obligation-fingerprint report)))
    (is (= (:transition-fingerprint ledger) (:transition-fingerprint report)))
    (is (= (:subject-fingerprint ledger) (:subject-fingerprint report)))
    (is (= :source-derived-union-of-whole-invariant-inferences
           (:proof-rule report)))
    (is (= :original-source-and-four-independent-restricted-checkers
           (:checking report)))
    (is (= :declared-abstract-state-projection-only (:scope report)))
    (is (= :all-pre-invariants-and-matching-transition-imply-post-invariant
           (:claim report)))
    (is (= #{:frame :literal :mixed :copy}
           (set (keys (:component-report-fingerprints report)))))
    (is (every? #(re-matches #"sha256:[0-9a-f]{64}" %)
                (vals (:component-report-fingerprints report))))
    (is (= ids (set (map :obligation-id outcomes))))
    (is (= (count ids) (count outcomes)))
    (is (= (count ids) (count (distinct (map :obligation-id outcomes)))))
    (is (= ids (set/union checked unsupported)))
    (is (empty? (set/intersection checked unsupported)))
    (is (= (:open-obligation-ids ledger)
           (:original-open-obligation-ids report)))
    (is (= :restricted-conditional-preservation-checked
           (:proof-status report)))
    (is (= :not-independently-proved (:checker-soundness report)))
    (is (contains? (:open-meta-obligations report)
                   :independent-proof-kernel-soundness))
    (is (contains? (:open-meta-obligations report)
                   :unified-preservation-dispatch-soundness))
    (is (contains? (:open-meta-obligations report)
                   :certificate-admission))
    (is (= :not-issued (:certificate report)))
    (is (= :not-admitted (:admission report)))
    (is (= :not-established (:runtime-enforcement report)))
    (is (= :not-established (:implementation-correspondence report)))
    (is (= :none (:authority report)))
    (is (not (contains? report :full-success?)))
    (is (not (contains? report :certified?)))
    (is (not (contains? report :transaction-authority)))
    report))

(deftest exact-coverage-of-every-declared-invariant
  (let [r (assert-unadmitted! baseline (check! baseline))]
    (is (= (preservation-ids expected-checked)
           (:conditionally-checked-preservation-ids r)))
    (is (= (preservation-ids expected-open)
           (:unsupported-preservation-ids r)))
    (is (= (set (keys rules)) (set (map :invariant (:preservation-outcomes r)))))))

(deftest each-supported-invariant-names-a-whole-family
  (let [r (check! baseline)]
    (doseq [id expected-checked]
      (let [o (outcome r id)]
        (is (= :checked-conditional-inference (:status o)) (str id))
        (is (contains? (set (:supporting-families o)) (:selected-family o)) (str id))
        (is (keyword? (:selected-inference o)) (str id))
        (is (re-matches #"sha256:[0-9a-f]{64}"
                        (:source-report-fingerprint o)) (str id))))))

(deftest unsupported-invariants-have-no-invented-provenance
  (let [r (check! baseline)]
    (doseq [id expected-open]
      (let [o (outcome r id)]
        (is (= :open (:status o)) (str id))
        (is (= [] (:supporting-families o)) (str id))
        (is (nil? (:selected-family o)) (str id))
        (is (nil? (:selected-inference o)) (str id))
        (is (nil? (:source-report-fingerprint o)) (str id))))))

(deftest first-family-is-selected-when-multiple-whole-proofs-exist
  (let [r (check! baseline)]
    (is (= :frame (:selected-family (outcome r :request/frame-version))))
    (is (= :literal (:selected-family (outcome r :request/literal-score))))
    (is (= :mixed (:selected-family (outcome r :request/mixed-version-status))))
    (is (= :copy (:selected-family (outcome r :request/copy-source))))
    (is (= :copy (:selected-family (outcome r :request/copy-with-frame))))))

(deftest conjunction-fragments-across-families-stay-open
  (let [r (check! baseline)]
    (doseq [id [:request/copy-and-literal-splice
                :request/copy-and-literal-reversed]]
      (is (= :open (:status (outcome r id))) (str id))
      (is (empty? (:supporting-families (outcome r id)))))))


(deftest each-family-can-establish-its-entire-single-invariant
  (doseq [[id expected-family]
          [[:request/frame-version :frame]
           [:request/literal-status :literal]
           [:request/mixed-version-status :mixed]
           [:request/copy-source :copy]
           [:request/copy-with-frame :copy]]]
    (let [r (check! (invariant-source id))
          o (outcome r id)]
      (is (= #{[:preservation id]}
             (:conditionally-checked-preservation-ids r)) (str id))
      (is (= [] (vec (:unsupported-preservation-ids r))) (str id))
      (is (= expected-family (:selected-family o)) (str id))
      (is (= 1 (count (:preservation-outcomes r))) (str id)))))

(deftest each-unsupported-single-invariant-remains-open
  (doseq [id expected-open]
    (let [r (check! (invariant-source id))]
      (is (= #{[:preservation id]} (:unsupported-preservation-ids r)) (str id))
      (is (empty? (:conditionally-checked-preservation-ids r)) (str id)))))

(deftest no-family-synthesizes-cross-field-inequalities
  (let [r (check! baseline)]
    (is (= :open (:status (outcome r :request/unsupported-arithmetic))))
    (is (= :open (:status (outcome r :request/unsupported-origin))))
    (is (= :open (:status (outcome r :request/false-literal))))))

(deftest optional-order-dependent-rule-remains-open
  (let [r (check! baseline)]
    (is (= :checked-conditional-inference
           (:status (outcome r :request/frame-optional))))
    (is (= :open (:status (outcome r :request/optional-dependent))))))

(deftest all-v1241-obligation-categories-remain-open
  (let [r (check! baseline)
        o (obligation/derive-obligations! baseline)]
    (is (= (set (keys (:obligations o)))
           #{:initial-state :preservation :assumption :implementation}))
    (is (= (:open-obligation-ids o) (:original-open-obligation-ids r)))
    (is (every? #(= :open (:status %))
                (mapcat val (:obligations o))))
    (is (not= (:open-obligation-ids o)
              (:unsupported-preservation-ids r)))
    (is (= :not-issued (:certificate r)))
    (is (= :none (:authority r)))))

(deftest adding-an-invariant-changes-complete-obligation-ledger
  (let [new-rule {:readers #{:request/status}
                  :expression [:not= [:read :request/status]
                               [:literal :claimed]]}
        changed (assoc-in baseline [:invariants :request/new] new-rule)
        original (check! baseline)
        extended (check! changed)]
    (is (= :open (:status (outcome extended :request/new))))
    (is (= (inc (count (:preservation-outcomes original)))
           (count (:preservation-outcomes extended))))
    (is (not= (:obligation-fingerprint original)
              (:obligation-fingerprint extended)))
    (is (not= (:fingerprint original) (:fingerprint extended)))
    (is (= (conj (:unsupported-preservation-ids original)
                 [:preservation :request/new])
           (:unsupported-preservation-ids extended)))))

(deftest removing-an-invariant-changes-the-obligation-ledger
  (let [short (update baseline :invariants dissoc :request/copy-source)
        orig (check! baseline)
        changed (check! short)]
    (is (nil? (outcome changed :request/copy-source)))
    (is (= (dec (count (:preservation-outcomes orig)))
           (count (:preservation-outcomes changed))))
    (is (not= (:fingerprint orig) (:fingerprint changed)))
    (is (not= (:original-open-obligation-ids orig)
              (:original-open-obligation-ids changed)))))

(deftest changed-guard-requires-a-new-unified-result
  (let [changed (assoc-in baseline [:construction :guard :expression]
                          [:= [:read :request/status] [:literal :done]])
        orig (check! baseline)
        updated (check! changed)]
    (is (not= (:transition-fingerprint orig)
              (:transition-fingerprint updated)))
    (is (not= (:obligation-fingerprint orig)
              (:obligation-fingerprint updated)))
    (is (not= (:fingerprint orig) (:fingerprint updated)))
    (is (= (:conditionally-checked-preservation-ids orig)
           (:conditionally-checked-preservation-ids updated)))))

(deftest changing-a-written-literal-can-remove-entire-invariant-support
  (let [changed (assoc-in baseline
                          [:construction :replacements :request/status]
                          [:literal :done])
        orig (check! baseline)
        updated (check! changed)]
    (is (= :checked-conditional-inference
           (:status (outcome orig :request/literal-status))))
    (is (= :open (:status (outcome updated :request/literal-status))))
    (is (= :open (:status (outcome updated :request/mixed-version-status))))
    (is (not= (:fingerprint orig) (:fingerprint updated)))))

(deftest changing-a-copy-source-can-remove-cross-field-proof
  (let [changed (-> baseline
                    (assoc-in [:construction :replacements :request/copied]
                              [:read :request/version])
                    (update-in [:construction :reads] conj :request/version))
        orig (check! baseline)
        updated (check! changed)]
    (is (= :checked-conditional-inference
           (:status (outcome orig :request/copy-source))))
    (is (= :open (:status (outcome updated :request/copy-source))))
    (is (= :open (:status (outcome updated :request/copy-pair))))
    (is (not= (:fingerprint orig) (:fingerprint updated)))))

(deftest changing-frame-to-written-requires-rederivation
  (let [changed (-> baseline
                    (update-in [:construction :frame] disj :request/version)
                    (update-in [:construction :writes] conj :request/version)
                    (assoc-in [:construction :replacements :request/version]
                              [:literal -10]))
        orig (check! baseline)
        updated (check! changed)]
    (is (= :checked-conditional-inference
           (:status (outcome orig :request/frame-version))))
    (is (= :open (:status (outcome updated :request/frame-version))))
    (is (= :open (:status (outcome updated :request/mixed-version-status))))
    (is (= :open (:status (outcome updated :request/copy-with-frame))))
    (is (not= (:fingerprint orig) (:fingerprint updated)))))

(deftest unchanged-source-produces-deterministic-identity
  (let [r (check! baseline)
        same (check! baseline)]
    (is (= r same))
    (is (= (:fingerprint r) (:fingerprint same)))
    (is (= (:component-report-fingerprints r)
           (:component-report-fingerprints same)))))

(deftest initial-condition-is-part-of-the-fingerprint
  (let [changed (assoc-in baseline [:initial-condition :expression]
                          [:= [:read :request/status] [:literal :done]])
        orig (check! baseline)
        updated (check! changed)]
    (is (not= (:obligation-fingerprint orig)
              (:obligation-fingerprint updated)))
    (is (not= (:fingerprint orig) (:fingerprint updated)))
    (is (= (:conditionally-checked-preservation-ids orig)
           (:conditionally-checked-preservation-ids updated)))
    (is (= :not-issued (:certificate updated)))))

(deftest changed-invariant-expression-is-not-a-reusable-result
  (let [changed (assoc-in baseline
                          [:invariants :request/frame-version :expression]
                          [:>= [:read :request/version] [:literal 20]])
        orig (check! baseline)
        updated (check! changed)]
    (is (not= (:obligation-fingerprint orig)
              (:obligation-fingerprint updated)))
    (is (not= (:fingerprint orig) (:fingerprint updated)))
    (is (= :checked-conditional-inference
           (:status (outcome updated :request/frame-version))))))

(deftest changed-operation-subject-invalidates-the-identity
  (let [changed (assoc-in baseline [:construction :subject :id]
                          :example/another-operation)
        orig (check! baseline)
        updated (check! changed)]
    (is (not= (:subject-fingerprint orig) (:subject-fingerprint updated)))
    (is (not= (:fingerprint orig) (:fingerprint updated)))
    (is (= :none (:authority updated)))))

(deftest copied-unified-report-cannot-be-passed-as-original-source
  (let [r (check! baseline)]
    (rejects! #(check! r)
              :gesso.assurance.proof.obligation/invalid-obligation-source
              :wrong-keys)))

(deftest invented-proof-flag-in-source-is-rejected
  (rejects! #(check! (assoc baseline :proved? true))
            :gesso.assurance.proof.obligation/invalid-obligation-source
            :wrong-keys))

(deftest invented-certificate-in-source-is-rejected
  (rejects! #(check! (assoc baseline :certificate :admitted))
            :gesso.assurance.proof.obligation/invalid-obligation-source
            :wrong-keys))

(deftest missing-source-invariants-are-rejected
  (rejects! #(check! (dissoc baseline :invariants))
            :gesso.assurance.proof.obligation/invalid-obligation-source
            :wrong-keys))

(deftest invalid-guard-is-rejected-before-unification
  (let [changed (assoc-in baseline [:construction :guard :expression]
                          [:literal :open])]
    (is (map? (rejection #(check! changed))))))

(deftest undeclared-assignment-reader-rejected-before-unification
  (let [changed (assoc-in baseline
                          [:construction :replacements :request/copied]
                          [:read :request/missing])]
    (rejects! #(check! changed)
              :gesso.assurance.proof.transition/invalid-transition
              :undeclared-assignment-reader)))

(deftest physically-observed-samples-do-not-promote-abstract-proof
  (let [r (check! baseline)]
    (is (= :not-established (:runtime-enforcement r)))
    (is (= :not-established (:implementation-correspondence r)))
    (is (= :not-independently-proved (:checker-soundness r)))
    (is (= :not-issued (:certificate r)))
    (is (= :not-admitted (:admission r)))))

;; Use with-redefs-fn instead of a hard-coded sibling namespace. That tests
;; every family on precisely the same original source and ledger boundary.
(def ^:private sibling-vars
  [#'frame/check-frame-preservation!
   #'literal/check-literal-assignment-preservation!
   #'mixed/check-mixed-preservation!
   #'copy/check-copy-preservation!])
(defn- tampered-sibling [sibling mutate]
  (let [original (var-get sibling)]
    (with-redefs-fn
      {sibling (fn [source] (mutate (original source)))}
      #(rejection (fn [] (check! baseline))))))
(defn- rejects-family! [sibling mutate]
  (let [d (tampered-sibling sibling mutate)]
    (is (= unified-error (:error/type d))
        (str sibling ": " (pr-str d)))
    (is (= :family-report-contract-changed (:reason d))
        (str sibling ": " (pr-str d)))))

(deftest all-four-families-reject-forged-authority
  (doseq [s sibling-vars]
    (rejects-family! s #(assoc % :authority :transaction))))

(deftest all-four-families-reject-forged-certification
  (doseq [s sibling-vars]
    (rejects-family! s #(assoc % :certificate :issued))
    (rejects-family! s #(assoc % :certified? true))))

(deftest all-four-families-reject-pretend-physical-correspondence
  (doseq [s sibling-vars]
    (rejects-family! s #(assoc % :implementation-correspondence :proved))))

(deftest all-four-families-reject-wrong-proof-rule
  (doseq [s sibling-vars]
    (rejects-family! s #(assoc % :proof-rule :anything-goes))))

(deftest all-four-families-reject-missing-preservation-outcomes
  (doseq [s sibling-vars]
    (rejects-family! s #(update % :preservation-outcomes pop))))

(deftest all-four-families-reject-duplicate-preservation-outcomes
  (doseq [s sibling-vars]
    (rejects-family! s #(update % :preservation-outcomes
                                conj (first (:preservation-outcomes %))))))

(deftest all-four-families-reject-forged-supported-id-set
  (let [id [:preservation :request/copy-and-literal-splice]]
    (doseq [s sibling-vars]
      (rejects-family! s #(update % :conditionally-checked-preservation-ids
                                  conj id)))))

(deftest all-four-families-reject-forged-source-fingerprint
  (doseq [s sibling-vars]
    (rejects-family! s #(assoc % :obligation-fingerprint
                               (str "sha256:" (apply str (repeat 64 "f")))))))

(deftest all-four-families-reject-changed-scope
  (doseq [s sibling-vars]
    (rejects-family! s #(assoc % :scope :universal-xtdb-refinement))))

(deftest all-four-families-reject-unproved-claimed-soundness
  (doseq [s sibling-vars]
    (rejects-family! s #(assoc % :checker-soundness :independently-proved))))

(deftest all-four-families-reject-omitted-original-obligations
  (doseq [s sibling-vars]
    (rejects-family! s #(assoc % :original-open-obligation-ids #{}))))

(deftest all-four-families-reject-mutated-outcome-reader-set
  (doseq [s sibling-vars]
    (rejects-family!
     s
     #(update % :preservation-outcomes
              (fn [outcomes]
                (assoc-in outcomes [0 :readers] #{:request/not-declared}))))))

(deftest forged-obligation-ledger-is-rejected-before-sibling-dispatch
  (let [orig obligation/derive-obligations!]
    (with-redefs [obligation/derive-obligations!
                  (fn [source]
                    (assoc (orig source) :proof-status :proved))]
      (rejects! #(check! baseline)
                unified-error :obligation-source-contract-changed))))

(deftest original-obligation-is-never-marked-as-discharged
  (let [r (check! baseline)]
    (is (set/subset? (:conditionally-checked-preservation-ids r)
                     (:original-open-obligation-ids r)))
    (is (not= :proved (:proof-status r)))
    (is (not (contains? r :discharged-obligation-ids)))
    (is (= :none (:authority r)))))

(defn- forge-one-complete-family-judgment
  "Keep the sibling's source fingerprints, counts and open ledger intact while
   fabricating support for an invariant its rule family cannot establish.
   Critically, the field derivations still belong to the old, OPEN judgment.
   A dispatcher claiming independent checking must not trust this summary."
  [r obligation-id]
  (-> r
      (update :preservation-outcomes
              (fn [outcomes]
                (mapv (fn [o]
                        (if (= obligation-id (:obligation-id o))
                          (assoc o :status :checked-conditional-inference
                                   :inference :fabricated-unsupported-whole-rule)
                          o))
                      outcomes)))
      (update :conditionally-checked-preservation-ids conj obligation-id)
      (update :unsupported-preservation-ids disj obligation-id)))

(deftest self-consistent-forged-whole-invariant-support-must-be-rejected
  (let [id [:preservation :request/copy-and-literal-splice]
        forged (tampered-sibling
                #'frame/check-frame-preservation!
                #(forge-one-complete-family-judgment % id))]
    ;; Deliberately RED against v1267: it accepts the fabricated whole-rule
    ;; judgment despite retaining the original OPEN derivation evidence.
    ;; Do not weaken this assertion to make the suite green.
    (is (= {:error/type unified-error
            :reason :family-report-contract-changed}
           (select-keys forged [:error/type :reason]))
        (str "v1267 accepted a fabricated frame proof: " (pr-str forged)))))

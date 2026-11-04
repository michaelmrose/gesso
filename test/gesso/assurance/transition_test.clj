(ns gesso.assurance.transition-test
  "v1240: permanent adversarial coverage of v1239's CLOSED typed guarded
   replacement constructor. These tests cover creation, updates, no-write
   transitions, exact read/write/frame footprints, optional field presence,
   malformed sources, unauthorized effects and downstream tampering.

   A successfully built transition is only COPYABLE, UNADMITTED source. These
   tests do not claim invariant proof, complete database effect containment,
   authorization, XTDB execution, or production correspondence."
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.assurance.transition :as build]
            [gesso.assurance.proof.transition :as abstract]
            [gesso.assurance.proof.derivation :as derivation]))

(def ^:private subject
  {:kind :operation :id :example/claim
   :interface {:operation :example/claim}})

(def ^:private claim
  {:subject subject
   :state-types
   {:request/status {:type :enum :name :request/status
                     :values #{:open :claimed :done}}
    :request/owner {:type :keyword :optional? true}
    :request/version {:type :integer}
    :request/note {:type :string :optional? true :nullable? true}}
   :input-types {:input/actor {:type :keyword}}
   :guard {:readers #{:request/status}
           :expression [:= [:read :request/status] [:literal :open]]}
   :reads #{:request/status :input/actor}
   :writes #{:request/status :request/owner}
   :replacements {:request/status [:literal :claimed]
                  :request/owner [:read :input/actor]}
   :frame #{:request/version :request/note}})

(def ^:private create-request
  {:subject {:kind :operation :id :example/create
             :interface {:operation :example/create}}
   :state-types
   {:request/exists {:type :boolean}
    :request/status {:type :enum :name :request/status
                     :values #{:missing :open}}
    :request/owner {:type :keyword :optional? true}
    :request/version {:type :integer}
    :request/created-at {:type :integer}
    :request/note {:type :string :optional? true}}
   :input-types {:input/actor {:type :keyword}}
   :guard {:readers #{:request/exists}
           :expression [:= [:read :request/exists] [:literal false]]}
   :reads #{:request/exists :input/actor}
   :writes #{:request/exists :request/status :request/owner :request/version}
   :replacements {:request/exists [:literal true]
                  :request/status [:literal :open]
                  :request/owner [:read :input/actor]
                  :request/version [:literal 1]}
   :frame #{:request/created-at :request/note}})

(def ^:private claim-before
  {:request/status :open :request/version 17})
(def ^:private claim-input {:input/actor :helper/one})
(def ^:private claim-after
  {:request/status :claimed :request/owner :helper/one :request/version 17})

(defn- error-data [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn- rejects-as!
  ([value reason] (rejects-as! value :gesso.assurance.transition/invalid-construction reason))
  ([value error-type reason]
   (let [data (error-data #(build/build-guarded-replacement! value))]
     (is (map? data) (str "Expected rejection " reason))
     (is (= error-type (:error/type data)) (pr-str data))
     (is (= reason (:reason data)) (pr-str data))
     (is (vector? (:at data)) (pr-str data))
     data)))

(defn- still-unadmitted! [report]
  (is (= build/construction-type (:type report)))
  (is (= build/transition-version (:version report)))
  (is (= :closed-source-and-typed-abstract-relation (:checking report)))
  (is (= :declared-abstract-state-projection-only (:construction-scope report)))
  (is (= :not-proved (:proof-status report)))
  (is (= :not-issued (:certificate report)))
  (is (= :not-admitted (:admission report)))
  (is (= :not-established (:runtime-enforcement report)))
  (is (= :not-established (:implementation-correspondence report)))
  (is (= :none (:authority report)))
  (is (= #{:initial-state-invariant
           :abstract-invariant-preservation
           :production-transition-correspondence
           :current-runtime-authorization
           :atomic-transaction-enforcement
           :physical-effect-containment}
         (:open-obligations report)))
  (is (not (contains? report :valid?)))
  (is (not (contains? report :transaction-authority)))
  report)

(defn- matches?
  [source pre in post]
  (:matches? (abstract/observe-transition! source pre in post)))

(deftest source-construction-is-exact-and-not-a-certificate
  (let [report (still-unadmitted! (build/build-guarded-replacement! claim))
        source (:transition report)
        description (abstract/describe-transition! source)]
    (is (= (dissoc claim :replacements :frame)
           (dissoc source :assignments)))
    (is (= (:replacements claim) (:assignments source)))
    (is (= #{:request/status :request/owner} (:writes report)))
    (is (= #{:request/status :input/actor} (:reads report)))
    (is (= #{:request/version :request/note} (:frame report)))
    (is (= (set (keys (:state-types claim))) (:state-projection report)))
    (is (= (:fingerprint description) (:transition-fingerprint report)))
    (is (= (:subject-fingerprint description) (:subject-fingerprint report)))
    (is (re-matches #"sha256:[0-9a-f]{64}"
                    (:transition-fingerprint report)))
    (is (= report (build/build-guarded-replacement! claim)))
    (is (matches? source claim-before claim-input claim-after))))

(deftest creation-uses-explicit-guard-and-all-four-declared-writes
  (let [report (still-unadmitted! (build/build-guarded-replacement!
                                   create-request))
        source (:transition report)
        before {:request/exists false :request/status :missing
                :request/version 0 :request/created-at 101}
        after {:request/exists true :request/status :open
               :request/owner :helper/two :request/version 1
               :request/created-at 101}]
    (is (= (:writes create-request) (:writes report)))
    (is (= (:frame create-request) (:frame report)))
    (is (matches? source before {:input/actor :helper/two} after))
    (is (false? (matches? source (assoc before :request/exists true)
                          {:input/actor :helper/two} after)))
    (is (false? (matches? source before {:input/actor :helper/two}
                          (assoc after :request/version 2))))
    (is (false? (matches? source before {:input/actor :helper/two}
                          (assoc after :request/created-at 102))))
    (is (false? (matches? source before {:input/actor :helper/two}
                          (assoc after :request/note "injected"))))))

(deftest update-assignment-of-an-optional-target-makes-it-present
  (let [source (:transition (build/build-guarded-replacement! claim))]
    (is (matches? source claim-before claim-input claim-after))
    (is (false? (matches? source claim-before claim-input
                          (dissoc claim-after :request/owner))))
    (is (false? (matches? source claim-before claim-input
                          (assoc claim-after :request/owner :helper/two))))
    (is (false? (matches? source claim-before claim-input
                          (assoc claim-after :request/version 18))))
    (is (false? (matches? source (assoc claim-before :request/status :claimed)
                          claim-input claim-after)))))

(deftest optional-framed-field-preserves-both-absence-and-nil
  (let [source (:transition (build/build-guarded-replacement! claim))
        nil-before (assoc claim-before :request/note nil)
        nil-after (assoc claim-after :request/note nil)]
    (is (matches? source claim-before claim-input claim-after))
    (is (matches? source nil-before claim-input nil-after))
    (is (false? (matches? source claim-before claim-input nil-after)))
    (is (false? (matches? source nil-before claim-input claim-after)))
    (is (false? (matches? source nil-before claim-input
                          (assoc claim-after :request/note "changed"))))))

(deftest read-only-construction-has-full-frame-and-no-assignment
  (let [read-only (-> claim
                      (assoc :input-types {}
                             :reads #{:request/status}
                             :writes #{}
                             :replacements {}
                             :frame (set (keys (:state-types claim)))))
        report (still-unadmitted! (build/build-guarded-replacement!
                                   read-only))
        source (:transition report)]
    (is (empty? (:writes report)))
    (is (= (set (keys (:state-types claim))) (:frame report)))
    (is (matches? source claim-before {} claim-before))
    (is (false? (matches? source claim-before {}
                          (assoc claim-before :request/status :claimed))))
    (is (false? (matches? source claim-before {}
                          (assoc claim-before :request/note nil))))))

(deftest source-field-preservation-includes-unrelated-optional-fields
  (let [extended (-> claim
                     (assoc-in [:state-types :request/audit]
                               {:type :string :optional? true})
                     (update :frame conj :request/audit))
        report (build/build-guarded-replacement! extended)
        source (:transition report)]
    (is (= #{:request/note :request/version :request/audit} (:frame report)))
    (is (matches? source claim-before claim-input claim-after))
    (is (false? (matches? source claim-before claim-input
                          (assoc claim-after :request/audit "undisclosed"))))))

(deftest builder-does-not-execute-user-functions
  (let [called (atom 0)
        effects (fn [& _] (swap! called inc))]
    (is (= :wrong-keys (:reason (rejects-as! (assoc claim :effect effects)
                                              :wrong-keys))))
    (is (= 0 @called))
    (is (= :wrong-keys (:reason (rejects-as! (assoc claim :callback effects)
                                              :wrong-keys))))
    (is (= 0 @called))))

(deftest undeclared-effects-and-evidence-are-rejected
  (doseq [extra [:effects :side-effects :delete :transaction
                 :callback :after :proof-status :certificate
                 :admission :valid? :authority :db-fx]]
    (testing (str "Unrecognized key " extra)
      (let [data (rejects-as! (assoc claim extra :forged) :wrong-keys)]
        (is (contains? (:unexpected data) extra))))))

(deftest closed-root-requires-all-and-only-declared-keys
  (doseq [key (keys claim)]
    (testing (str "Missing required " key)
      (let [data (rejects-as! (dissoc claim key) :wrong-keys)]
        (is (contains? (:missing data) key)))))
  (rejects-as! [] :expected-plain-map)
  (rejects-as! (into (sorted-map) claim) :expected-plain-map)
  (rejects-as! (with-meta claim {:claimed-proof true})
               :metadata-not-allowed))

(deftest read-write-and-frame-sets-must-be-plain-qualified-sets
  (doseq [key [:reads :writes :frame]]
    (testing (name key)
      (rejects-as! (assoc claim key (vec (get claim key)))
                   :expected-plain-field-set)
      (rejects-as! (assoc claim key (with-meta (get claim key)
                                      {:trusted? true}))
                   :metadata-not-allowed)
      (rejects-as! (update claim key conj :unqualified)
                   :invalid-field))))

(deftest replacements-must-be-plain-closed-qualified-field-map
  (rejects-as! (assoc claim :replacements []) :expected-replacement-map)
  (rejects-as! (assoc claim :replacements (into (sorted-map)
                                               (:replacements claim)))
               :expected-replacement-map)
  (rejects-as! (assoc claim :replacements
                      (with-meta (:replacements claim) {:proof true}))
               :metadata-not-allowed)
  (rejects-as! (update claim :replacements assoc :owner [:literal :helper/one])
               :invalid-field))

(deftest state-declarations-are-closed-to-nonplain-maps-and-metadata
  (rejects-as! (assoc claim :state-types []) :expected-state-type-map)
  (rejects-as! (assoc claim :state-types
                      (into (sorted-map) (:state-types claim)))
               :expected-state-type-map)
  (rejects-as! (assoc claim :state-types
                      (with-meta (:state-types claim) {:trusted true}))
               :metadata-not-allowed))

(deftest every-declared-write-must-have-one-replacement
  (let [missing (rejects-as! (update claim :replacements dissoc :request/owner)
                             :write-footprint-mismatch)
        extra (rejects-as! (update claim :replacements
                                   assoc :request/version [:literal 18])
                           :write-footprint-mismatch)]
    (is (= #{:request/owner} (:unexpected missing)))
    (is (= #{:request/version} (:missing extra))))
  (rejects-as! (assoc claim :writes #{:request/status})
               :write-footprint-mismatch)
  (rejects-as! (update claim :writes conj :request/undeclared)
               :writes-outside-state))

(deftest undeclared-state-fields-cannot-be-added-as-writes
  (rejects-as! (update claim :writes conj :request/injected)
               :writes-outside-state)
  (rejects-as! (update claim :replacements assoc :request/injected
                       [:literal :bad])
               :write-footprint-mismatch)
  (rejects-as! (-> claim
                   (update :writes conj :request/injected)
                   (update :replacements assoc :request/injected
                           [:literal :bad]))
               :writes-outside-state))

(deftest frame-must-be-exact-complement-of-writes
  (let [missing (rejects-as! (update claim :frame disj :request/note)
                             :frame-footprint-mismatch)
        injected (rejects-as! (update claim :frame conj :request/unknown)
                              :frame-footprint-mismatch)
        written (rejects-as! (update claim :frame conj :request/owner)
                             :frame-footprint-mismatch)]
    (is (= #{:request/note} (:missing missing)))
    (is (= #{:request/unknown} (:unexpected injected)))
    (is (= #{:request/owner} (:unexpected written))))
  (rejects-as! (assoc claim :frame #{}) :frame-footprint-mismatch)
  (rejects-as! (assoc claim :frame
                      (set (keys (:state-types claim))))
               :frame-footprint-mismatch))

(deftest read-footprint-matches-guard-plus-assignment-sources
  (let [missing (rejects-as! (assoc claim :reads #{:request/status})
                             :gesso.assurance.proof.transition/invalid-transition
                             :read-footprint-mismatch)
        extra (rejects-as! (update claim :reads conj :request/version)
                           :gesso.assurance.proof.transition/invalid-transition
                           :read-footprint-mismatch)]
    (is (= #{:input/actor} (:missing missing)))
    (is (= #{:request/version} (:unexpected extra))))
  (rejects-as! (assoc-in claim [:replacements :request/owner]
                         [:read :request/unknown])
               :gesso.assurance.proof.transition/invalid-transition
               :undeclared-assignment-reader)
  (rejects-as! (assoc claim :guard
                      {:readers #{:request/missing}
                       :expression [:= [:read :request/missing]
                                    [:literal :open]]})
               :gesso.assurance.proof.transition/invalid-transition
               :unknown-guard-reader))

(deftest assignment-language-rejects-arbitrary-effects
  (doseq [form [[:execute :input/actor]
                [:delete :request/status]
                [:call :client/fetch]
                [:update :request/status]
                [:read :input/actor :extra]
                [:literal :claimed :extra]]]
    (rejects-as! (assoc-in claim [:replacements :request/owner] form)
                 :gesso.assurance.proof.transition/invalid-transition
                 :unsupported-assignment))
  (rejects-as! (assoc-in claim [:replacements :request/owner]
                         (list :read :input/actor))
               :gesso.assurance.proof.transition/invalid-transition
               :expected-assignment-vector))

(deftest incompatible-types-and-optional-sources-reject
  (rejects-as! (assoc-in claim [:replacements :request/owner]
                         [:literal 99])
               :gesso.assurance.rule-type/invalid-rule-type
               :incompatible-equality-types)
  (rejects-as! (assoc-in claim [:replacements :request/owner]
                         [:read :request/owner])
               :gesso.assurance.proof.transition/invalid-transition
               :optional-assignment-source)
  (rejects-as! (assoc-in claim [:input-types :input/unread]
                         {:type :integer})
               :gesso.assurance.proof.transition/invalid-transition
               :unused-input-declarations))

(deftest original-abstract-observation-rejects-hidden-frame-mutation
  (let [source (:transition (build/build-guarded-replacement! claim))
        mutants [(assoc claim-after :request/version 18)
                 (assoc claim-after :request/note nil)
                 (assoc claim-after :request/note "secret")
                 (dissoc claim-after :request/owner)
                 (assoc claim-after :request/status :done)
                 (assoc claim-after :request/owner :helper/other)]]
    (is (matches? source claim-before claim-input claim-after))
    (doseq [candidate mutants]
      (testing (pr-str candidate)
        (is (false? (matches? source claim-before claim-input candidate)))))))

(deftest no-magic-admission-on-result-tampering
  (let [report (build/build-guarded-replacement! claim)
        forged (assoc report :proof-status :proved
                             :certificate :issued
                             :admission :admitted
                             :authority :execute
                             :valid? true)]
    (is (= :none (:authority report)))
    (is (not= forged report))
    ;; Caller-controlled reports are neither proof sources nor admitted
    ;; evidence. The independently checked v1235 boundary requires ORIGINAL
    ;; transition source, and does not accept a decorated construction report.
    (let [data (try (derivation/check-transition-derivation! forged)
                    nil
                    (catch clojure.lang.ExceptionInfo e (ex-data e)))]
      (is (qualified-keyword? (:error/type data)))
      (is (some? (:reason data))))))

(deftest changing-semantics-invalidates-fingerprint
  (let [base (build/build-guarded-replacement! claim)
        new-status (build/build-guarded-replacement!
                    (assoc-in claim [:replacements :request/status]
                              [:literal :done]))
        extended (build/build-guarded-replacement!
                  (-> claim
                      (assoc-in [:state-types :request/tag]
                                {:type :string :optional? true})
                      (update :frame conj :request/tag)))
        new-subject (build/build-guarded-replacement!
                     (assoc claim :subject
                            {:kind :operation :id :example/second-claim
                             :interface {:operation :example/second-claim}}))]
    (is (not= (:transition-fingerprint base)
              (:transition-fingerprint new-status)))
    (is (not= (:transition-fingerprint base)
              (:transition-fingerprint extended)))
    (is (not= (:transition-fingerprint base)
              (:transition-fingerprint new-subject)))
    (is (not= (:subject-fingerprint base)
              (:subject-fingerprint new-subject)))
    (is (= (:subject-fingerprint base)
           (:subject-fingerprint new-status)))))

(deftest constructor-source-remains-usable-by-original-derivation-checker
  (let [report (build/build-guarded-replacement! claim)
        source (:transition report)
        checked (derivation/check-transition-derivation!
                 {:transition source
                  :steps [{:id :step/guard :rule :guard
                           :conclusion [:guard]}]
                  :goal [:guard]})]
    (is (= (:transition-fingerprint report)
           (:transition-fingerprint checked)))
    (is (= :none (:authority checked)))
    (still-unadmitted! report)))

(deftest finite-frame-mutation-matrix
  (let [source (:transition (build/build-guarded-replacement! claim))
        baseline (assoc claim-before :request/note nil)
        good (assoc claim-after :request/note nil)
        versions [0 1 16 18 99]
        notes ["" "changed" "x" "other"]]
    (is (matches? source baseline claim-input good))
    (doseq [version versions
            note notes]
      (is (false? (matches? source baseline claim-input
                            (assoc good :request/version version
                                        :request/note note)))))))

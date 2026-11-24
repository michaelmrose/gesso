(ns gesso.assurance.release.decision-test
  "v2260: Permanent positive and adversarial coverage for the v2259
   selected-release *diagnostic* evaluator. These tests establish the
   evaluator's bounded fail-closed behavior when supplied fabricated trusted
   callbacks. They do not authenticate a provider, certify the original
   application's source or physical writers, activate a deployment, or
   discharge R5.1 Stage 11 release admission. Real installed-boundary
   integration and external evidence qualification remain separate work."
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.assurance.release.decision :as decision]))

(def ^:private requirements
  {:requirement/authority :capability/claim
   :requirement/writers :capability/claim
   :requirement/browser :capability/view
   :requirement/release :capability/claim})

(def ^:private phase-requirements
  {:candidate :requirement/authority
   :installation :requirement/writers
   :activation :requirement/browser
   :completion :requirement/release})

(defn- gate [phase kind]
  {:id (keyword (name phase) (name kind))
   :requirement (get phase-requirements phase)
   :kind kind
   :class (if (= kind :negative) :qualification :structural)
   :mechanism :gesso/selected-check
   :mechanism-version "checker-v1"
   :scope :scope/selected-release
   :allowed-premises #{}
   :expected-boundary (when (= kind :negative)
                        :boundary/no-unadmitted-effect)})

(def ^:private selected-gates
  (into {} (for [phase decision/phases]
             [phase [(gate phase :positive) (gate phase :negative)]])))

(def ^:private intent
  {:claim-id :claim/selected-release
   :application-id :application/proving-example
   :milestone :milestone/single-node
   :profile-id :profile/selected
   :capabilities #{:capability/claim :capability/view}
   :requirements requirements
   :subjects {:candidate "candidate-hash"
              :installation "installation-hash"
              :activation "activation-hash"
              :completion "completion-hash"}
   :gates selected-gates})

(defn- inventory []
  {:capabilities (:capabilities intent)
   :requirements (set (keys (:requirements intent)))
   :gates (into #{} (map :id) (mapcat identity (vals selected-gates)))})

(defn- phase-subject [phase]
  {:phase phase
   :claim-id (:claim-id intent)
   :subject-id (get-in intent [:subjects phase])})

(defn- passed-evidence [{:keys [claim-id phase gate subject]}]
  {:status :passed
   :claim-id claim-id
   :phase phase
   :gate-id (:id gate)
   :requirement-id (:requirement gate)
   :subject-id (:subject-id subject)
   :mechanism (:mechanism gate)
   :mechanism-version (:mechanism-version gate)
   :scope (:scope gate)
   :evidence-id (str "evidence-" (name phase) "-" (name (:kind gate)))
   :premises #{}
   :observed-boundary (:expected-boundary gate)
   :forbidden-effects (when (= :negative (:kind gate)) :absent)})

(defn- options []
  {:intent intent
   :discover! inventory
   :subject! phase-subject
   :run-gate! passed-evidence})

(defn- with-run-gate [callback]
  (assoc (options) :run-gate! callback))

(defn- rejection [thunk]
  (try
    (thunk)
    ::not-rejected
    (catch clojure.lang.ExceptionInfo e
      (if (:release-decision/rejection (ex-data e))
        (:reason (ex-data e))
        [:unexpected-ex-info (ex-data e)]))))

(defn- blocked-at [report phase gate-id expected]
  (is (= :blocked (:status report)))
  (is (= :blocked (get-in report [:decisions phase :status])))
  (is (= expected (get-in report [:decisions phase :blockers gate-id])))
  (is (= :none (:release-authority report)))
  (is (= :none (:runtime-effect-permission report))))

(deftest valid-evidence-has-no-release-or-application-authority
  (let [report (decision/assess! (options))]
    (is (= decision/report-type (:type report)))
    (is (= decision/report-version (:version report)))
    (is (= :selected-checks-passed-only (:status report)))
    (is (= #{:requirement/authority :requirement/writers
             :requirement/browser :requirement/release}
           (:requirements report)))
    (is (= (set (map :id (mapcat identity (vals selected-gates))))
           (:required-gates report)))
    (is (= :matched (get-in report [:initial-inventory :status])))
    (is (= :matched (get-in report [:final-inventory :status])))
    (is (= (zipmap decision/phases (repeat :matched)) (:final-subjects report)))
    (is (= (set decision/phases) (set (keys (:decisions report)))))
    (is (= 8 (reduce + (map (comp count :rows) (vals (:decisions report))))))
    (doseq [phase decision/phases]
      (is (= :checks-passed-only (get-in report [:decisions phase :status])))
      (is (= #{} (get-in report [:decisions phase :accepted-premises])))
      (is (= #{} (set (keys (get-in report [:decisions phase :blockers])))))
      (is (= #{:passed}
             (set (map :status (vals (get-in report [:decisions phase :rows])))))))
    (is (= {} (:blockers report)))
    (is (= #{} (:accepted-premises report)))
    (is (false? (:conditional? report)))
    (doseq [[key expected]
            {:release-authority :none
             :candidate-admission :not-issued
             :installation-eligibility :not-issued
             :activation-eligibility :not-issued
             :completion-claim :not-issued
             :runtime-effect-permission :none}]
      (is (= expected (get report key)) (str "no credential: " key)))))

(deftest all-phases-perform-fresh-observations-without-skipping-mandatory-gates
  (let [trace (atom [])
        report (decision/assess!
                {:intent intent
                 :discover! (fn [] (swap! trace conj [:discover]) (inventory))
                 :subject! (fn [phase]
                             (swap! trace conj [:subject phase])
                             (phase-subject phase))
                 :run-gate! (fn [{:keys [phase gate] :as args}]
                              (swap! trace conj [:gate phase (:id gate)])
                              (passed-evidence args))})
        event-count (frequencies @trace)]
    (is (= :selected-checks-passed-only (:status report)))
    (is (= 2 (get event-count [:discover])))
    (is (= 8 (count (filter #(= :gate (first %)) @trace))))
    ;; Once before and after each phase; reread each completed predecessor
    ;; before evaluating any following phase, plus one final observation.
    (doseq [[phase expected-count]
            {:candidate 6 :installation 5 :activation 4 :completion 3}]
      (is (= expected-count (get event-count [:subject phase]))))
    (is (= [:discover] (first @trace)))
    (is (= [[:discover] [:subject :candidate] [:subject :installation]
            [:subject :activation] [:subject :completion]]
           (vec (take-last 5 @trace))))))

(deftest malformed-options-reject-before-any-callback
  (let [calls (atom 0)
        fail-if-called (fn [& _] (swap! calls inc) (throw (Exception. "called")))
        opts {:intent intent :discover! fail-if-called
              :subject! fail-if-called :run-gate! fail-if-called}
        bad-options [(dissoc opts :discover!)
                     (assoc opts :extra true)
                     (assoc opts :subject! nil)
                     (assoc opts :run-gate! :untrusted/keyword)
                     (with-meta opts {:forged true})
                     [] nil]]
    (doseq [o bad-options]
      (is (= :invalid-assessment-input
             (rejection #(decision/assess! o)))))
    (is (zero? @calls))))

(deftest fixed-denominator-and-distinct-subjects-reject-invalid-intents
  (let [cases [(assoc intent :capabilities #{})
               (assoc intent :requirements {})
               (assoc intent :capabilities #{:capability/claim})
               (assoc intent :requirements
                      {:requirement/authority :capability/unknown})
               (dissoc intent :profile-id)
               (assoc intent :claim-id :unqualified)
               (assoc intent :subjects
                      (dissoc (:subjects intent) :completion))
               (assoc-in intent [:subjects :activation] "candidate-hash")
               (assoc-in intent [:subjects :candidate] "")
               (assoc-in intent [:subjects :completion] "ab")
               (update intent :gates dissoc :completion)
               (assoc-in intent [:gates :completion] [])
               (assoc-in intent [:gates :candidate] nil)
               (assoc intent :extra-field true)
               (with-meta intent {:forged true})]]
    (doseq [case cases]
      (is (= :invalid-or-incomplete-intent
             (rejection #(decision/assess! (assoc (options) :intent case))))))))

(deftest gate-declarations-require-exact-fields-and-distinct-identities
  (let [bad [(update-in intent [:gates :candidate 0] dissoc :mechanism)
             (assoc-in intent [:gates :candidate 0 :mechanism] :unqualified)
             (assoc-in intent [:gates :candidate 0 :kind] :neutral)
             (assoc-in intent [:gates :candidate 0 :class] :arbitrary)
             (assoc-in intent [:gates :candidate 0 :requirement]
                       :requirement/not-declared)
             (assoc-in intent [:gates :candidate 0 :expected-boundary]
                       :boundary/not-allowed)
             (assoc-in intent [:gates :candidate 1 :expected-boundary] nil)
             (assoc-in intent [:gates :candidate 0 :allowed-premises]
                       #{:unqualified})
             (assoc-in intent [:gates :candidate 0 :scope] "not-a-keyword")
             (update-in intent [:gates :candidate 0] assoc :extra true)
             (update-in intent [:gates :candidate 0]
                        with-meta {:opaque true})]]
    (doseq [case bad]
      (is (= :invalid-gate-declaration
             (rejection #(decision/assess! (assoc (options) :intent case))))))
    (is (= :duplicate-required-gate
           (rejection
            #(decision/assess!
              (assoc (options) :intent
                     (assoc-in intent [:gates :installation 0 :id]
                               :candidate/positive))))))))

(deftest original-requirements-need-coverage-and-adversarial-controls
  (is (= :uncovered-original-requirement
         (rejection #(decision/assess!
                      (assoc (options) :intent
                             (assoc-in intent [:requirements :requirement/extra]
                                       :capability/claim))))))
  (is (= :missing-positive-or-adversarial-control
         (rejection #(decision/assess!
                      (assoc (options) :intent
                             (update intent :gates
                                     (fn [gs] (into {} (for [[phase gates] gs]
                                                        [phase (vec (filter
                                                                     (fn [gate] (= :positive (:kind gate)))
                                                                     gates))]))))))))))

(deftest missing-or-extra-original-source-inventory-blocks-every-phase
  (doseq [[label mutate]
          [[:missing-capability #(update % :capabilities disj :capability/view)]
           [:extra-capability #(update % :capabilities conj :capability/unknown)]
           [:missing-requirement #(update % :requirements disj :requirement/writers)]
           [:extra-requirement #(update % :requirements conj :requirement/extra)]
           [:missing-gate #(update % :gates disj :activation/positive)]
           [:extra-gate #(update % :gates conj :activation/unknown)]]]
    (testing (name label)
      (let [calls (atom 0)
            report (decision/assess!
                    (assoc (options)
                           :discover! (fn [] (mutate (inventory)))
                           :run-gate! (fn [arg]
                                        (swap! calls inc)
                                        (passed-evidence arg))))]
        (is (= :blocked (:status report)))
        (is (= :inventory-diverged (get-in report [:initial-inventory :status])))
        (is (= :not-examined (get-in report [:decisions :candidate :status])))
        (is (= :not-current-or-incomplete (get-in report [:blockers :source-inventory])))
        (is (zero? @calls))))))

(deftest unavailable-and-malformed-source-inventory-never-qualify
  (doseq [callback [(fn [] (throw (ex-info "source offline" {})))
                    (fn [] {:capabilities #{} :requirements #{} :gates []})
                    (fn [] (with-meta (inventory) {:untrusted true}))
                    (fn [] (assoc (inventory) :authorization :forged))]]
    (let [report (decision/assess! (assoc (options) :discover! callback))]
      (is (= :blocked (:status report)))
      (is (not= :matched (get-in report [:initial-inventory :status])))
      (is (= :not-examined (get-in report [:decisions :candidate :status])))))
  (let [reads (atom 0)
        report (decision/assess!
                (assoc (options) :discover!
                       (fn [] (if (= 2 (swap! reads inc))
                                (update (inventory) :gates disj :completion/negative)
                                (inventory)))))]
    (is (= 2 @reads))
    (is (= :checks-passed-only
           (get-in report [:decisions :completion :status])))
    (is (= :blocked (:status report)))
    (is (= :inventory-diverged (get-in report [:final-inventory :status])))
    (is (= :not-current-or-incomplete
           (get-in report [:blockers :source-inventory])))))

(deftest a-nonpassing-gate-blocks-at-its-original-phase
  (doseq [phase decision/phases
          status [:failed :incomplete :unavailable :unsupported :skipped
                  :stale :missing]]
    (let [calls (atom [])
          report (decision/assess!
                  (with-run-gate
                    (fn [{actual-phase :phase gate :gate :as arg}]
                      (swap! calls conj [actual-phase (:id gate)])
                      (if (and (= phase actual-phase)
                               (= :negative (:kind gate)))
                        {:status status}
                        (passed-evidence arg)))))
          next-phases (rest (drop-while #(not= % phase) decision/phases))]
      (blocked-at report phase (keyword (name phase) "negative") status)
      (doseq [next-phase next-phases]
        (is (= :not-examined
               (get-in report [:decisions next-phase :status]))))
      (is (= (* 2 (inc (.indexOf decision/phases phase))) (count @calls)))
      (is (not-any? (fn [[actual-phase _]] (some #{actual-phase} next-phases)) @calls)))))

(deftest unexpected-status-or-passed-without-evidence-blocks
  (doseq [[replacement expected]
          [[{:status :green} :invalid-result]
           [{:status :passed} :invalid-evidence]
           [{:status :passed :claim-id :claim/selected-release} :invalid-evidence]
           [nil :invalid-result]
           [true :invalid-result]]]
    (let [report (decision/assess!
                  (with-run-gate
                    (fn [{:keys [phase gate] :as arg}]
                      (if (and (= :candidate phase)
                               (= :negative (:kind gate)))
                        replacement
                        (passed-evidence arg)))))]
      (blocked-at report :candidate :candidate/negative expected)))
  (let [report (decision/assess!
                (with-run-gate
                  (fn [{:keys [phase gate] :as arg}]
                    (if (and (= :candidate phase)
                             (= :negative (:kind gate)))
                      {:status :passed}
                      (passed-evidence arg)))))]
    ;; :passed is a recognized status, but without the required evidence
    ;; tuple it is invalid evidence, not a success.
    (blocked-at report :candidate :candidate/negative :invalid-evidence)))

(deftest forged-negative-evidence-is-rejected-field-by-field
  (doseq [[label mutation]
          [[:claim #(assoc % :claim-id :claim/other)]
           [:phase #(assoc % :phase :installation)]
           [:gate #(assoc % :gate-id :candidate/positive)]
           [:requirement #(assoc % :requirement-id :requirement/release)]
           [:subject #(assoc % :subject-id "another-subject")]
           [:mechanism #(assoc % :mechanism :checker/foreign)]
           [:mechanism-version #(assoc % :mechanism-version "version-2")]
           [:scope #(assoc % :scope :scope/foreign)]
           [:no-evidence #(dissoc % :evidence-id)]
           [:short-evidence #(assoc % :evidence-id "x")]
           [:unknown-premise #(assoc % :premises #{:premise/undisclosed})]
           [:missing-boundary #(assoc % :observed-boundary nil)]
           [:wrong-boundary #(assoc % :observed-boundary :boundary/wrong)]
           [:forbidden-effects-unknown #(assoc % :forbidden-effects :unknown)]
           [:forbidden-effects-present #(assoc % :forbidden-effects :present)]
           [:additional-field #(assoc % :duplicate-certificate true)]
           [:metadata #(with-meta % {:forged true})]]]
    (testing (name label)
      (let [report (decision/assess!
                    (with-run-gate
                      (fn [{:keys [phase gate] :as arg}]
                        (let [result (passed-evidence arg)]
                          (if (and (= phase :candidate)
                                   (= :negative (:kind gate)))
                            (mutation result)
                            result)))))]
        (blocked-at report :candidate :candidate/negative
                    (if (= :metadata label) :invalid-result :invalid-evidence))))))

(deftest positive-evidence-may-not-silently-claim-an-adversarial-observation
  (doseq [mutation [#(assoc % :observed-boundary :boundary/no-unadmitted-effect)
                    #(assoc % :forbidden-effects :absent)
                    #(assoc % :premises #{:premise/foreign})]]
    (let [report (decision/assess!
                  (with-run-gate
                    (fn [{:keys [phase gate] :as arg}]
                      (if (and (= phase :candidate)
                               (= :positive (:kind gate)))
                        (mutation (passed-evidence arg))
                        (passed-evidence arg)))))]
      (blocked-at report :candidate :candidate/positive :invalid-evidence))))

(deftest changing-subject-during-a-phase-invalidates-that-phase
  (doseq [phase decision/phases]
    (let [reads (atom {})
          report (decision/assess!
                  (assoc (options) :subject!
                         (fn [actual-phase]
                           (let [n (get (swap! reads update actual-phase
                                               (fnil inc 0)) actual-phase)]
                             (cond-> (phase-subject actual-phase)
                               (and (= phase actual-phase) (= n 2))
                               (assoc :subject-id "retired-subject"))))))]
      (is (= :blocked (:status report)))
      (is (= :subject-changed
             (get-in report [:decisions phase :blockers :subject])))
      (doseq [later (rest (drop-while #(not= % phase) decision/phases))]
        (is (= :not-examined
               (get-in report [:decisions later :status])))))))

(deftest prior-phase-subject-loss-blocks-later-decisions
  (let [candidate-reads (atom 0)
        report (decision/assess!
                (assoc (options) :subject!
                       (fn [phase]
                         (cond-> (phase-subject phase)
                           (and (= phase :candidate)
                                (>= (swap! candidate-reads inc) 3))
                           (assoc :subject-id "retired-candidate")))))]
    (is (= :blocked (:status report)))
    (is (= :checks-passed-only (get-in report [:decisions :candidate :status])))
    (is (= :prior-subject-changed
           (get-in report [:decisions :installation :blockers :predecessor])))
    (is (= :not-examined (get-in report [:decisions :activation :status])))))

(deftest conditional-proofs-stay-conditional-and-undeclared-premises-fail
  (let [conditioned-intent
        (assoc-in intent [:gates :candidate 0 :allowed-premises]
                  #{:premise/deployed-version :premise/environment})
        opts (assoc (options) :intent conditioned-intent)
        passed (fn [{:keys [gate] :as args}]
                 (cond-> (passed-evidence args)
                   (= :candidate/positive (:id gate))
                   (assoc :premises #{:premise/environment})))
        report (decision/assess! (assoc opts :run-gate! passed))]
    (is (= :selected-checks-passed-only (:status report)))
    (is (true? (:conditional? report)))
    (is (= #{:premise/environment} (:accepted-premises report)))
    (is (= #{:premise/environment}
           (get-in report [:decisions :candidate :accepted-premises])))
    (is (= :none (:release-authority report)))
    (let [bad (decision/assess!
               (assoc opts :run-gate!
                      (fn [{:keys [gate] :as args}]
                        (cond-> (passed-evidence args)
                          (= :candidate/positive (:id gate))
                          (assoc :premises #{:premise/unissued})))))]
      (blocked-at bad :candidate :candidate/positive :invalid-evidence))))

(deftest callback-exceptions-are-recorded-not-treated-as-green
  (let [report (decision/assess!
                (assoc (options) :run-gate!
                       (fn [{:keys [gate] :as args}]
                         (if (= :candidate/negative (:id gate))
                           (throw (IllegalStateException. "checker stopped"))
                           (passed-evidence args)))))]
    (blocked-at report :candidate :candidate/negative :unavailable)
    (is (= "java.lang.IllegalStateException"
           (get-in report [:decisions :candidate :rows :candidate/negative
                           :error-class]))))
  (let [report (decision/assess!
                (assoc (options) :subject!
                       (fn [phase]
                         (if (= :candidate phase)
                           (throw (IllegalStateException. "provider offline"))
                           (phase-subject phase)))))]
    (is (= :blocked (:status report)))
    (is (= :subject-unavailable
           (get-in report [:decisions :candidate :blockers :subject])))))

(deftest repeated-evaluations-do-not-reuse-a-historical-green-report
  (let [active? (atom true)
        opts (assoc (options) :discover!
                    (fn [] (if @active?
                             (inventory)
                             (update (inventory) :gates disj :completion/negative))))
        first-report (decision/assess! opts)]
    (is (= :selected-checks-passed-only (:status first-report)))
    (reset! active? false)
    (let [new-report (decision/assess! opts)]
      (is (= :blocked (:status new-report)))
      (is (= :not-examined (get-in new-report [:decisions :candidate :status])))
      (is (= :none (:release-authority first-report)))
      (is (= :none (:release-authority new-report))))))

(deftest final-subject-drift-cannot-borrow-historical-green-phase-rows
  (doseq [when-drift [:completion-gate :final-inventory]
          phase [:candidate :installation :activation]
          failure [:changed :unavailable]]
    (let [drift? (atom false)
          reads (atom 0)
          calls (atom 0)
          report (decision/assess!
                  (assoc (options)
                         :discover! (fn []
                                      (when (and (= 2 (swap! reads inc))
                                                 (= :final-inventory when-drift))
                                        (reset! drift? true))
                                      (inventory))
                         :subject! (fn [p]
                                     (if (and @drift? (= p phase))
                                       (if (= :changed failure)
                                         (assoc (phase-subject p) :subject-id "changed-subject")
                                         (throw (ex-info "subject unavailable" {})))
                                       (phase-subject p)))
                         :run-gate! (fn [args]
                                      (swap! calls inc)
                                      (when (and (= :completion-gate when-drift)
                                                 (= :completion/negative (get-in args [:gate :id])))
                                        (reset! drift? true))
                                      (passed-evidence args))))
          expected (if (= :changed failure) :subject-diverged :subject-unavailable)]
      (is (= 8 @calls))
      (is (= :matched (get-in report [:final-inventory :status])))
      (is (every? #(= :checks-passed-only (:status %)) (vals (:decisions report))))
      (is (= :blocked (:status report)))
      (is (= {phase expected} (get-in report [:blockers :final-subjects])))
      (is (= (assoc (zipmap decision/phases (repeat :matched)) phase expected)
             (:final-subjects report)))
      (is (= :none (:release-authority report))))))

(deftest partial-release-evidence-reobserves-acquired-subjects
  (doseq [drift-phase [:candidate :installation]
          drift? [false true]]
    (let [final? (atom false)
          reads (atom 0)
          report (decision/assess!
                  (assoc (options)
                         :discover! (fn []
                                      (when (= 2 (swap! reads inc))
                                        (reset! final? true))
                                      (inventory))
                         :subject! (fn [phase]
                                     (when-not (= :activation phase)
                                       (cond-> (phase-subject phase)
                                         (and @final? drift? (= drift-phase phase))
                                         (assoc :subject-id "retired-subject"))))))
          expected (cond-> {:candidate :matched :installation :matched}
                     drift? (assoc drift-phase :subject-diverged))]
      (is (= :blocked (:status report)))
      (is (= :checks-passed-only (get-in report [:decisions :candidate :status])))
      (is (= :checks-passed-only (get-in report [:decisions :installation :status])))
      (is (= :blocked (get-in report [:decisions :activation :status])))
      (is (= :not-examined (get-in report [:decisions :completion :status])))
      (is (= expected (:final-subjects report)))
      (is (= (when drift? {drift-phase :subject-diverged})
             (get-in report [:blockers :final-subjects])))
      (is (= :none (:release-authority report))))))

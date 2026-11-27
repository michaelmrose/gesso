(ns gesso.assurance.reference-issued-envelope-argument
  "Join source-relative envelopes to an issued local recovery observation."
  (:require [gesso.assurance.inductive-report :as report]
            [gesso.assurance.reference-mapped-decoder :as decoder]
            [gesso.assurance.reference-mapped-outcome :as outcome]
            [gesso.assurance.reference-mapped-submission :as submission]
            [gesso.assurance.reference-source-envelope-argument :as argument]
            [gesso.assurance.xtdb-envelope :as envelope]
            [gesso.live.outbox.model-publication :as publication]
            [gesso.model.inductive-plan :as induction]))

(def profile :conditional-issued-source-envelope-recovery-v1)
(def ^:private boundaries
  {#'publication/read-framed-submission-observation! publication/read-framed-submission-observation!
   #'publication/framed-submission-origin-matches? publication/framed-submission-origin-matches?
   #'submission/compare-observed! submission/compare-observed!
   #'induction/require-current-mapped-admission! induction/require-current-mapped-admission!
   #'induction/recheck-mapped-induction! induction/recheck-mapped-induction!
   #'decoder/compare-issued-closed! decoder/compare-issued-closed!
   #'outcome/derive-cases! outcome/derive-cases!
   #'induction/mapped-transition-evidence! induction/mapped-transition-evidence!
   #'argument/check-instance! argument/check-instance!})

(def ^:private base
  {:profile profile :authority :none :certificate :not-issued
   :provider-refinement :not-established :universal-refinement :not-established
   :writer-coverage :not-established :proof-discharge :not-performed
   :physical-prestate-provenance :not-established :command-authentication :not-established})

(defn- require! [condition reason]
  (when-not condition (throw (ex-info "Unqualified issued envelope recovery" {:reason reason}))))

(defn- check-boundaries! []
  (require! (every? (fn [[v f]] (identical? @v f)) boundaries)
            :issued-envelope-boundary-replaced))

(defn compare-recovery!
  "Qualify the selected commit/throw/commit recovery window against the exact
   issued admission and source. Obtain consumed vectors from the issued claim.
   Supplied preceding rows remain premises; matching the consumed preparation
   witness does not authenticate their physical origin or every snapshot field.
   Historical diagnostic only: no submission or authority."
  [connectable admission source declarations steps handle]
  (try
    (check-boundaries!)
    (require! (publication/framed-submission-origin-matches? admission handle connectable)
              :unissued-envelope-origin)
    (let [description (induction/require-current-mapped-admission! admission source)
          fp (select-keys description [:catalog-fingerprint :induction-fingerprint])
          abstract (induction/recheck-mapped-induction! admission)
          closed (decoder/compare-issued-closed! admission source declarations)
          cases (:cases (outcome/derive-cases! source declarations))
          comparison (submission/compare-observed! admission steps handle)
          {:keys [claims]} (publication/read-framed-submission-observation! admission handle)]
      (require! (and (= report/report-type (:type abstract))
                     (= report/report-version (:version abstract))
                     (= :original-sources-and-both-live-handles-rechecked (:checking abstract))
                     (= :source-bound-conditional-abstract-safety (:verdict abstract))
                     (= :closed-declared-abstract-machine-only (:scope abstract))
                     (= :all-original-obligations-remain-open (get-in abstract [:classification :original-obligations :status]))
                     (= :not-performed (:assumption-discharge abstract))
                     (= :not-established (:production-certification abstract))
                     (= :none (:authority abstract) (:transaction-authority abstract))
                     (= fp (select-keys (:identity abstract) (keys fp)))
                     (= (:original-open-obligations description) (get-in abstract [:classification :original-obligations :by-operation]))
                     (= :matched (:status closed))
                     (= :matched-issued-closed-construction (:closed-decoder-binding closed))
                     (= fp (select-keys closed (keys fp))))
                :unqualified-issued-induction)
      (require! (and (= :matched (:status comparison))
                     (= :matched-local-issued-policy-window (:observation-origin comparison))
                     (= :matched-observed-checked-candidates (:framed-handoffs comparison))
                     (= :matched-selected-retained-operations (:adapter-payloads comparison))
                     (vector? claims) (= 3 (count claims) (count steps)))
                :unqualified-issued-recovery-window)
      (let [instances
            (mapv (fn [step {:keys [submission checked]}]
                    (let [{:keys [docs journal spec context source-id prefix-count]} step
                          id (:alternative spec)
                          bound (:tx-ops submission)
                          witness (induction/mapped-transition-evidence! admission checked)
                          original (some #(when (= id (:id %)) (:construction %))
                                         (get-in source [:inductive-source :catalog :operations]))
                          expected-context (assoc fp :selectors (get-in description [:input-derivations id]))]
                      (require! (and original (= original (:original step))
                                     (= (:target-map description) (:targets step))
                                     (= (:publication-derivations description) (:declarations step))
                                     (= (:residual-assertion-derivations description) (:residual-declarations step))
                                     (= expected-context context) (= cases (:outcome-cases spec))
                                     (= witness (apply dissoc (peek (peek bound)) envelope/binding-keys))
                                     (= :exact-generated-prefix (get-in checked [:candidate :frame-prefix])))
                                :issued-envelope-preparation-mismatch)
                      (let [result (argument/check-instance! source declarations (:catalog-fingerprint fp) cases id
                                                            {:docs docs :journal journal :spec spec :context context
                                                             :source-id source-id :prefix-count prefix-count
                                                             :admitted (subvec bound 1 (dec (count bound)))
                                                             :witness witness :bound bound :consumed bound})]
                        (require! (= :conditional-source-envelope-instance (:status result))
                                  :issued-source-envelope-mismatch)
                        (assoc result :induction-fingerprint-provenance :matched-original-live-handles
                               :consumption-correspondence :matched-local-issued-policy-window
                               :preparation-correspondence :matched-consumed-candidate-witness))))
                  steps claims)]
        (check-boundaries!)
        (require! (publication/framed-submission-origin-matches? admission handle connectable)
                  :unissued-envelope-origin)
        (assoc base :status :conditional-issued-source-envelope-recovery
               :induction-identity (:identity abstract) :instances instances
               :remaining-conditions (into #{} (mapcat :remaining-conditions) instances))))
    (catch clojure.lang.ExceptionInfo e
      (assoc base :status :not-established :reason (or (:reason (ex-data e)) :unqualified-issued-envelope)))))

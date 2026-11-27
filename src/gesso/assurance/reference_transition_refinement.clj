(ns gesso.assurance.reference-transition-refinement
  "Conditional local simulation for the closed mapped command fragment.
   Supplied row images are not authenticated provider observations."
  (:require [gesso.assurance.reference-mapped-writes :as writes]
            [gesso.assurance.reference-recovery-projection :as projection]
            [gesso.assurance.proof.mapped-prestate-semantics :as prestate]
            [gesso.assurance.proof.independent-transition-semantics :as transition]
            [gesso.assurance.proof.typed-value-semantics :as values]
            [gesso.assurance.transition :as construction]))

(def profile :conditional-closed-mapped-transition-refinement-v1)
(def ^:private original-writes writes/derive-transition!)
(def ^:private original-frequencies writes/put-row-frequencies!)
(def ^:private original-prestate prestate/describe!)
(def ^:private original-projection projection/project!)
(def ^:private original-observe values/observe-state!)
(def ^:private original-build construction/build-guarded-replacement!)
(def ^:private original-transition transition/observe-transition!)

(defn- boundaries-current? []
  (and (identical? original-writes writes/derive-transition!)
       (identical? original-frequencies writes/put-row-frequencies!)
       (identical? original-prestate prestate/describe!)
       (identical? original-projection projection/project!)
       (identical? original-observe values/observe-state!)
       (identical? original-build construction/build-guarded-replacement!)
       (identical? original-transition transition/observe-transition!)
       (projection/boundaries-current?)))

(defn- reject! [reason]
  (throw (ex-info "Unqualified closed mapped transition" {:reason reason})))

(defn- row-image! [original coordinates docs journal]
  ;; The existing closed-row predicate checker rejects unrelated targets,
  ;; duplicate slots and missing required documents before indexing.
  (original-prestate original coordinates docs journal)
  (into {:gesso_commands (into {} (map (juxt :xt/id identity)) journal)}
        (map (fn [[table rows]] [table (into {} (map (juxt :xt/id identity)) rows)])) docs))

(defn- supplied-image! [original coordinates image]
  (when-not (and (map? image) (contains? image :gesso_commands)
                 (every? (fn [[_ rows]]
                           (and (map? rows) (every? (fn [[id row]] (and (map? row) (= id (:xt/id row)))) rows))) image))
    (reject! :invalid-supplied-row-image))
  (let [docs (into {} (map (fn [[table rows]] [table (vec (vals rows))])) (dissoc image :gesso_commands))
        journal (vec (vals (:gesso_commands image)))]
    (when-not (= image (row-image! original coordinates docs journal))
      (reject! :invalid-supplied-row-image)))
  image)

(defn- put-image! [before suffix]
  ;; The closed suffix is ordinary default-valid-time puts, one row per
  ;; physical identity. No deletes, explicit valid ranges or repeated targets.
  (when-not (and (vector? suffix) (seq suffix)) (reject! :invalid-mapped-put-suffix))
  (let [rows (mapcat (fn [[op table & rows :as put]]
                      (when-not (and (vector? put) (= :put-docs op)
                                     (contains? before table) (seq rows)
                                     (every? #(and (map? %) (string? (:xt/id %))) rows))
                        (reject! :invalid-mapped-put-suffix))
                      (map (fn [row] [[table (:xt/id row)] row]) rows)) suffix)]
    (when-not (= (count rows) (count (set (map first rows))))
      (reject! :duplicate-physical-put-target))
    (reduce (fn [image [address row]] (assoc-in image address row)) before rows)))

(defn- typed-projection! [original coordinates image cases]
  (let [state (original-projection coordinates image cases)
        types (:state-types original)]
    (when-not (and (= (set (keys types)) (set (keys state)))
                   (= (set (keys types)) (:present-readers (original-observe types state))))
      (reject! :incomplete-typed-projection))
    state))

(defn check-step!
  "Check one source-derived mapped put suffix and supplied actual row images.
   For a commit the successful full prestate guard is modeled by equality
   with the prepared image; independent exact puts must produce the entire
   post image and its projection must satisfy the original abstract relation.
   For an abort the attempted suffix still qualifies against the prepared
   image, but actual images must stutter. An abort need not have a false
   abstract guard: the provider may abort for another reason.

   This is a source-relative instance of a conditional local simulation,
   not proof of SQL guard semantics, atomic provider execution, attribution,
   valid-time continuity or complete admitted-writer coverage."
  [original coordinates docs journal spec suffix commitment before after]
  (let [base {:profile profile :scope :supplied-closed-mapped-command-rows-only
              :provider-refinement :not-established :writer-coverage :not-established
              :universal-refinement :not-established :proof-discharge :not-performed
              :authority :none :certificate :not-issued}]
    (try
      (when-not (and (boundaries-current?) (#{:committed :aborted} commitment)
                     (map? (:outcome-cases spec)) (seq (:outcome-cases spec)))
        (reject! :unqualified-transition-boundary))
      (let [prepared (row-image! original coordinates docs journal)
            before (supplied-image! original coordinates before)
            after (supplied-image! original coordinates after)
            expected (original-writes original coordinates docs journal spec)
            prepared-post (put-image! prepared suffix)
            expected-post (put-image! prepared (:puts expected))
            cases (:outcome-cases spec)
            pre (typed-projection! original coordinates before cases)
            post (typed-projection! original coordinates after cases)]
        (when-not (and (= (original-frequencies suffix)
                          (original-frequencies (:puts expected)))
                       (= prepared-post expected-post)
                       (= (:before expected) (typed-projection! original coordinates prepared cases))
                       (= (:after expected) (typed-projection! original coordinates prepared-post cases)))
          (reject! :source-derived-put-mismatch))
        (case commitment
          :committed
          (do (when-not (= prepared before) (reject! :committed-prestate-guard-mismatch))
              (when-not (= prepared-post after) (reject! :committed-row-effect-mismatch))
              (when-not (and (= pre (:before expected)) (= post (:after expected))
                             (true? (:matches? (original-transition
                                                (:transition (original-build original))
                                                pre (:inputs spec) post))))
                (reject! :abstract-transition-mismatch)))
          :aborted
          (when-not (= before after) (reject! :aborted-row-effect-mismatch)))
        (when-not (boundaries-current?) (reject! :transition-boundary-replaced))
        (assoc base :status :conditional-step-correspondence
               :alternative-id (get-in original [:subject :id]) :commitment commitment
               :relation (if (= commitment :committed) :committed-abstract-transition :aborted-stutter)
               :checked-row-effect (if (= commitment :committed) :exact-source-derived-puts :exact-row-stutter)
               :before pre :after post
               :conditions #{:faithful-closed-prestate-guard-semantics
                             :faithful-ordered-atomic-default-valid-time-puts
                             :faithful-abort-atomicity
                             :faithful-command-and-envelope-attribution
                             :faithful-authoritative-projection-and-expression-semantics
                             :complete-admitted-writer-and-temporal-boundary-coverage}))
      (catch clojure.lang.ExceptionInfo e
        (assoc base :status :not-established :reason (or (:reason (ex-data e)) :unqualified-transition))))))

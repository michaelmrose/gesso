(ns gesso.assurance.proof.executed-guard-authorization-correspondence
  "v1388: Correlate a source-derived abstract guard's READER footprint with
   a registered predicate-contract callback and the incorporation plan observed
   during one executor-controlled typed submission.

   This is a process-local, bounded diagnostic. An application-supplied reader
   -> exact-check link is NOT an independently discovered authorization policy;
   predicate-contract! is trusted application code, and the prepared plan is
   NOT a transaction-internal XTDB assertion receipt. A rejected, missing, or
   altered observation is never promoted to a successful authorization result.

   All nine physical-refinement obligations remain open. No certificate,
   admission, transaction authority, or proof of policy equivalence is issued.
   The nested :controlled-result may contain sensitive command/plan data and
   must be confined to trusted server-side diagnostics. The guard report itself
   exposes counts, not document IDs, values, link tuples, or raw callbacks."
  (:require [clojure.set :as set]
            [gesso.assurance.proof.executor-controlled-acquisition :as controlled]
            [gesso.assurance.proof.executable-writer-correspondence-obligations :as writers]
            [gesso.model.tx-model :as model]
            [gesso.model.tx :as tx]))

(def guard-report-type
  :gesso.assurance.proof.executed-guard-authorization-correspondence/one-attempt)
(def guard-report-version 1)

(def ^:private source-keys #{:controlled-source :guard-reader-links})
(def ^:private controlled-keys
  #{:writer-source :operation-id :binding :configuration :input-paths})
(def ^:private original-run controlled/run-controlled-typed-submit!)
(def ^:private original-writers writers/derive-executable-writer-obligations!)
(def ^:private original-admitted model/admitted-plan)
(def ^:private original-guard-target tx/guard-target)
(def ^:private maximum-readers 128)
(def ^:private maximum-checks 256)

(defn- reject! [reason at]
  (throw (ex-info "Invalid executed-guard correspondence request."
                  {:executed-guard-authorization-correspondence/rejection true
                   :reason reason :at at})))

(defn- plain-map? [x]
  (or (instance? clojure.lang.PersistentArrayMap x)
      (instance? clojure.lang.PersistentHashMap x)))

(defn- exact! [x keys at]
  (when-not (and (plain-map? x) (nil? (meta x))
                 (= keys (set (clojure.core/keys x))))
    (reject! :invalid-source-shape at))
  x)

(defn- original-functions! []
  (when-not (and (identical? original-run
                             (var-get #'controlled/run-controlled-typed-submit!))
                 (identical? original-writers
                             (var-get #'writers/derive-executable-writer-obligations!))
                 (identical? original-admitted
                             (var-get #'model/admitted-plan))
                 (identical? original-guard-target
                             (var-get #'tx/guard-target)))
    (reject! :substituted-upstream-checker [])))

(defn- declared-guard! [controlled-source]
  ;; v1368 rechecks source-derived issued writers and the induction catalog.
  ;; In particular, IDs and fingerprints are not themselves policy proofs.
  (let [writer-source (:writer-source controlled-source)
        operation-id (:operation-id controlled-source)
        ledger (original-writers writer-source)
        row (first (filter #(= operation-id (:operation-id %))
                           (:operation-rows ledger)))
        catalog (:induction-catalog writer-source)
        declarations (filterv #(= operation-id (:id %)) (:operations catalog))
        abstract (get row :abstract-contract)]
    (when-not (and (= writers/report-type (:type ledger))
                   (= writers/report-version (:version ledger))
                   (= 1 (count declarations))
                   (:declared-abstract-operation? row)
                   (:checked-issued-executor? row)
                   (= :not-established (:cross-source-transition-equivalence row))
                   (= :not-established (:executable-refinement row))
                   (false? (:all-executable-refinement-obligations-discharged? ledger))
                   (= :none (:authority ledger))
                   (= :not-issued (:certificate ledger))
                   (= (get-in (first declarations) [:declaration :guard :readers])
                      (:guard-readers abstract))
                   (= (get-in (first declarations) [:declaration :guard :expression])
                      (:guard-expression abstract)))
      (reject! :unbound-or-unsupported-abstract-guard [:controlled-source]))
    (:guard-readers abstract)))

(defn- guard-links! [links readers]
  ;; Each reader must have a nonempty, closed claimed physical predicate link.
  ;; The relationship itself is trusted application input, not proof that an
  ;; abstract reader actually comes from this document or that policy holds.
  (when-not (and (seq readers)
                 (plain-map? links) (nil? (meta links))
                 (<= (count readers) maximum-readers)
                 (= readers (set (keys links))))
    (reject! :incomplete-abstract-guard-links [:guard-reader-links]))
  (let [groups (vals links)
        checks (mapcat seq groups)]
    (when-not (and (every? #(and (set? %) (seq %) (nil? (meta %))) groups)
                   (<= (count checks) maximum-checks)
                   (= (count checks) (count (set checks)))
                   (every? (fn [check]
                             (and (vector? check) (nil? (meta check))
                                  (= 4 (count check))
                                  (keyword? (nth check 0))
                                  (some? (nth check 1))
                                  (keyword? (nth check 2))
                                  (some? (nth check 3))))
                           checks))
      (reject! :invalid-or-ambiguous-guard-links [:guard-reader-links]))
    (set checks)))

(defn- plan-guard-checks [plan]
  ;; Do not mistake update-CAS conditions for separately declared dependency
  ;; guards. Only canonical Gesso model :guards are in this comparison.
  (into #{}
        (mapcat (fn [guard]
                  (let [[table id] (original-guard-target guard)]
                    (map (fn [[field value]] [table id field value])
                         (get-in guard [:model/expected :model/checks])))))
        (:guards (original-admitted plan))))

(defn- guard-diagnostic [result observed links]
  (let [acquired (:acquisition-report result)
        complete? (and (= :complete-paired-hook-observation
                          (:capture-status result))
                       (map? acquired)
                       (#{:committed :rejected}
                        (:captured-physical-status acquired)))
        observed-once? (= 1 (:events observed))
        declaration (:declaration observed)
        shape? (and (plain-map? declaration)
                    (= #{:required-guard-checks :allowed-guard-checks
                         :required-update-checks :allowed-update-checks}
                       (set (keys declaration))))
        required (when shape? (:required-guard-checks declaration))
        allowed (when shape? (:allowed-guard-checks declaration))
        actual (when complete?
                 (try (plan-guard-checks (:captured-prepared-plan acquired))
                      (catch Exception _ nil)))
        agreed? (and complete? observed-once? shape?
                     (set? required) (set? allowed) (set? actual)
                     (= links required)
                     (set/subset? required actual)
                     (set/subset? actual allowed)
                     (= :matched
                        (get-in result [:typed-input-observation
                                        :construction-site-correlation])))
        status (cond
                 (not complete?) :no-complete-transaction-observation
                 (not observed-once?) :missing-or-duplicate-predicate-callback
                 (not agreed?) :abstract-link-or-guard-plan-divergence
                 :else :linked-required-guards-in-prepared-plan)]
    {:status status
     :linked-abstract-reader-count (:reader-count observed)
     :linked-check-count (count links)
     :predicate-callback-count (:events observed)
     :required-predicate-count (when (set? required) (count required))
     :prepared-guard-check-count (when (set? actual) (count actual))
     :callback-observed-before-construction? observed-once?
     :complete-paired-plan-observed? (boolean complete?)
     :linked-guard-plan-agreement? (boolean agreed?)}))

(defn run-guard-corresponded-typed-submit!
  "Run one v1384 controlled typed submission and correlate its independently
   extracted ABSTRACT guard reader names with a trusted, exact reader-to-check
   mapping and the predicate-contract! actually invoked before :construct!.

   Input is exactly {:controlled-source <v1384 full source>
                     :guard-reader-links {abstract-reader #{[table id field value]}}}.

   A missing or divergent callback/observation produces a negative diagnostic,
   NEVER a post-commit exception, revised authoritative resolution, retry, or
   certificate. Input/callback validation errors can reject before execution.
   Neither successful linking nor an observed XTDB rejection proves that an
   abstract authorization expression is semantically equivalent to the
   actual transaction guard."
  [source ctx]
  (exact! source source-keys [])
  (original-functions!)
  (let [{:keys [controlled-source guard-reader-links]} source
        _ (exact! controlled-source controlled-keys [:controlled-source])
        config (:configuration controlled-source)
        _ (when-not (and (plain-map? config)
                         (fn? (:predicate-contract! config))
                         (:construction-contract config)
                         (fn? (:target-contract! config)))
            (reject! :missing-closed-predicate-registration
                     [:controlled-source :configuration]))
        readers (declared-guard! controlled-source)
        linked (guard-links! guard-reader-links readers)
        capture (atom {:events 0 :declaration nil})
        original-predicate (:predicate-contract! config)
        wrapper (fn [callback-ctx binding]
                  (let [declared (original-predicate callback-ctx binding)]
                    (swap! capture (fn [{:keys [events]}]
                                     {:events (inc events)
                                      :declaration declared}))
                    declared))
        wrapped (assoc-in controlled-source
                          [:configuration :predicate-contract!] wrapper)
        ;; Exactly one call to the original controlled executor. Its normal
        ;; error/authorization/commit behavior is not changed by diagnostics.
        result (original-run wrapped ctx)
        observed (assoc @capture :reader-count (count readers))
        diagnostic (guard-diagnostic result observed linked)]
    {:type guard-report-type
     :version guard-report-version
     :checking :abstract-reader-links-vs-executed-predicate-callback-and-prepared-plan
     :operation-id (:operation-id controlled-source)
     :attempt-id (get-in result [:acquisition-report :attempt-id])
     :guard-observation diagnostic
     :source-of-abstract-guard :fresh-source-rechecked-induction-catalog
     :reader-to-physical-check-origin :trusted-application-supplied-links
     :predicate-declaration-origin :registered-application-callback
     :prepared-plan-origin :controlled-process-local-observation
     :actual-guard-evaluation-by-xtdb :not-established
     :abstract-guard-to-authorization-policy-equivalence :not-established
     :guard-authorization-obligation :open
     :complete-writer-mediation :not-established
     :universal-execution-refinement :not-established
     :physical-refinement-obligations (:physical-refinement-obligations result)
     :proof-status :guard-plan-correlation-diagnostic-only
     :admission :not-admitted
     :certificate :not-issued
     :transaction-authority :none
     :authority :none
     :controlled-result result}))

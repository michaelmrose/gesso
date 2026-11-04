(ns gesso.assurance.diagnostic
  "v1313: stable, FIRST-BLOCKER diagnostics, derived only from CURRENT original
   sources through the existing Gesso assurance checkers.

   (diagnose! {:kind :induction             :source <original catalog>})
   (diagnose! {:kind :inductive-report      :source <v1286 source>})
   (diagnose! {:kind :application-induction :source <v1294 source>})
   (diagnose! {:kind :observed-correspondence :source <v1302 source>})
   (diagnose! {:kind :finite-model          :source <v1309 finite source>})

   This is an EXPLANATION facility, never an admission path. It rechecks the
   original sources, not caller-supplied reports. A positive finite run is
   bounded and a positive observed comparison is unauthenticated. The output
   is COPYABLE and grants no certificate, runtime or transaction authority.
   Host inventory and source freshness are only relative to supplied inputs;
   independent checker soundness remains explicitly open."
  (:require [clojure.set :as set]
            [gesso.assurance.application-induction :as application]
            [gesso.assurance.inductive-report :as inductive-report]
            [gesso.assurance.observed-correspondence :as observed]
            [gesso.assurance.proof.finite-model-oracle :as finite]
            [gesso.assurance.proof.induction :as induction]))

(def diagnostic-version 1)
(def diagnostic-type :gesso.assurance.diagnostic/first-blocker)

(def ^:private kinds
  #{:induction :inductive-report :application-induction
    :observed-correspondence :finite-model})

(def ^:private evidence-classes
  {:induction :unadmitted-source-derived-abstract
   :inductive-report :source-relative-conditional-induction
   :application-induction :source-relative-application-assembly
   :observed-correspondence :unauthenticated-observed-application-step
   :finite-model :bounded-finite-universe-analysis})

(def ^:private fingerprint-keys
  [:catalog-fingerprint :induction-fingerprint :certificate-fingerprint
   :dependency-fingerprint :transition-fingerprint :subject-fingerprint])

(def ^:private always-open
  #{:independent-induction-checker-soundness
    :host-authoritative-entry-point-inventory-is-complete
    :physical-write-path-coverage
    :physical-prestate-and-initialization-correspondence
    :abstract-to-physical-transition-refinement
    :deployed-router-and-executors-adopt-checked-snapshot})

(def ^:private original-checkers
  {:induction induction/check-induction!
   :inductive-report inductive-report/build-inductive-report!
   :application-induction application/check-application-induction!
   :observed-correspondence observed/check-observed-correspondence!
   :finite-model finite/check-finite-model!})

(def ^:private public-checker-vars
  {:induction #'induction/check-induction!
   :inductive-report #'inductive-report/build-inductive-report!
   :application-induction #'application/check-application-induction!
   :observed-correspondence #'observed/check-observed-correspondence!
   :finite-model #'finite/check-finite-model!})

(defn- reject! [reason at]
  (throw (ex-info "Invalid Gesso diagnostic request."
                  {:error/type ::invalid-diagnostic-input
                   :reason reason :at at})))

(defn- plain-map? [x]
  (or (instance? clojure.lang.PersistentArrayMap x)
      (instance? clojure.lang.PersistentHashMap x)))

(defn- stable-order [values] (sort-by pr-str values))

(defn- blocker [class code at details]
  (merge {:class class :code code :at at} details))

(defn- result [kind evidence primary extra-open]
  (let [open (set/union always-open (or extra-open #{}))]
    {:type diagnostic-type
     :version diagnostic-version
     :kind kind
     :checking (if (= :invalid-source (:class primary))
                 :source-validation-rejected
                 :current-original-source-rechecked)
     :status :blocked
     :first-blocker primary
     :open-obligations open
     :open-premises (or (:open-premises evidence)
                        (get-in evidence [:classification :certificate :open-premises])
                        #{})
     :original-open-obligation-ids-by-operation
     (or (:original-open-obligation-ids-by-operation evidence)
         (get-in evidence [:classification :original-obligations :by-operation])
         {})
     :source-fingerprints (merge (select-keys (:identity evidence) fingerprint-keys)
                                 (select-keys evidence fingerprint-keys))
     :evidence-class (evidence-classes kind)
     :evidence-status :diagnostic-only
     :independent-checker-soundness :not-established
     :physical-write-path-completeness :not-established
     :universal-xtdb-refinement :not-established
     :production-certification :not-established
     :transaction-authority :none
     :authority :none}))

(defn- index-of-operation [catalog op]
  (first (keep-indexed (fn [i row] (when (= op (:id row)) i))
                       (:operations catalog))))

(defn- induction-blocker [report catalog prefix]
  (some
   (fn [{:keys [invariant initial-status unsupported-operation-ids]}]
     (cond
       (= :open initial-status)
       (blocker :abstract-induction :initial-consequence-open
                (into prefix [:invariants invariant])
                {:invariant-id invariant :obligation-id [:initial-state invariant]})

       (seq unsupported-operation-ids)
       (let [op (first (stable-order unsupported-operation-ids))
             i (index-of-operation catalog op)]
         (blocker :abstract-induction :preservation-open
                  (if (some? i)
                    (into prefix [:operations i :construction])
                    (into prefix [:operations]))
                  {:invariant-id invariant :operation-id op
                   :obligation-id [:preservation invariant]}))))
   (sort-by (comp pr-str :invariant) (:induction-outcomes report))))

(defn- from-induction [source evidence]
  (let [first-open (induction-blocker evidence source [:source])
        primary (or first-open
                    (blocker :admission :conditional-induction-not-admitted
                             [:source]
                             {:detail :unadmitted-abstract-induction-candidate}))]
    (result :induction evidence primary
            (:open-meta-obligations evidence))))

(defn- report-open-blocker [catalog evidence]
  (some
   (fn [step]
     (let [id (:invariant-id step)
           initial (get-in step [:initial :status])
           preserving (:preservation-by-operation step)]
       (cond
         (= :open initial)
         (blocker :abstract-induction :initial-consequence-open
                  [:source :catalog :invariants id]
                  {:invariant-id id :obligation-id [:initial-state id]})

         (seq (for [[_ {:keys [status]}] preserving :when (= :open status)] true))
         (let [op (first (stable-order
                          (for [[op {:keys [status]}] preserving
                                :when (= :open status)] op)))]
           (blocker :abstract-induction :preservation-open
                    (let [i (index-of-operation catalog op)]
                      (if (some? i)
                        [:source :catalog :operations i :construction]
                        [:source :catalog :operations]))
                    {:invariant-id id :operation-id op
                     :obligation-id [:preservation id]})))))
   (sort-by (comp pr-str :invariant-id)
            (get-in evidence [:classification :open-abstract :steps]))))

(defn- from-inductive-report [source evidence]
  (let [primary
        (or (report-open-blocker (:catalog source) evidence)
            (when-not (get-in evidence [:classification :certificate :fingerprint])
              (blocker :admission :conditional-induction-certificate-not-admitted
                       [:source :certificate-handle] {}))
            (when (= :not-established (:source-dependency-closure evidence))
              (blocker :dependency-closure :exact-dependency-closure-not-established
                       [:source :dependency-handle] {}))
            (blocker :physical-correspondence :physical-write-path-unproved
                     [:source :catalog] {}))
        open (get-in evidence [:classification :open :assumptions])]
    (result :inductive-report evidence primary open)))

(defn- from-application [source evidence]
  (let [extras (:observed-unclassified-entry-point-ids evidence)
        primary (if (seq extras)
                  (blocker :entry-point-coverage :unclassified-entry-point
                           [:source :authoritative-coverage-source
                            :enumerate-entry-points!]
                           {:entry-point-id (first (stable-order extras))})
                  (blocker :physical-correspondence :physical-write-path-unproved
                           [:source :authoritative-coverage-source
                            :enumerate-entry-points!]
                           {:detail :host-inventory-is-not-provably-complete}))]
    (result :application-induction evidence primary (:open-obligations evidence))))

(defn- from-observed [source evidence]
  (let [failed (first (stable-order
                       (for [[check ok?] (:observed-checks evidence)
                             :when (false? ok?)] check)))
        extras (:unclassified-entry-point-ids evidence)
        primary (cond
                  failed
                  (blocker :observed-transition :physical-transition-diverges
                           [:source :transition-source :observation]
                           {:failed-check failed
                            :operation-id (:operation evidence)})
                  (seq extras)
                  (blocker :entry-point-coverage :unclassified-entry-point
                           [:source :application-source :authoritative-coverage-source
                            :enumerate-entry-points!]
                           {:entry-point-id (first (stable-order extras))})
                  :else
                  (blocker :observation-provenance
                           :physical-observation-not-authenticated
                           [:source :transition-source :observation]
                           {:operation-id (:operation evidence)}))]
    (result :observed-correspondence evidence primary
            (:open-obligations evidence))))

(defn- from-finite [source evidence]
  (let [status (:status evidence)
        witness (:counterexample evidence)
        primary
        (case status
          :counterexample
          (blocker :finite-counterexample
                   (if (= :base (:kind witness)) :initial-counterexample
                       :preservation-counterexample)
                   (if (= :base (:kind witness))
                     [:source :catalog :invariants (:invariant-id witness)]
                     [:source :catalog :operations])
                   {:counterexample witness})
          :bounded-no-counterexample
          (blocker :finite-only :finite-model-does-not-prove-universal-induction
                   [:source :state-domains]
                   {:all-type-domains-complete?
                    (get-in evidence [:universe :all-type-domains-complete?])})
          :budget-exceeded
          (blocker :finite-inconclusive :finite-enumeration-budget-exceeded
                   [:source :max-cases]
                   {:reason (:reason evidence)})
          :unsupported-language
          (blocker :finite-inconclusive :finite-semantics-unsupported
                   (into [:source] (or (:at evidence) []))
                   {:reason (:reason evidence)})
          :invalid-source
          (blocker :invalid-source :finite-input-invalid
                   (into [:source] (or (:at evidence) []))
                   {:reason (:reason evidence)})
          (reject! :unknown-oracle-status [:source]))]
    (result :finite-model evidence primary
            #{:finite-model-does-not-prove-universal-induction})))

(defn- source-rejection [kind ex]
  (let [{:keys [reason at] :as data} (ex-data ex)
        upstream-type (:error/type data)]
    ;; Unexpected user/host exceptions are not assurance diagnostics. Never
    ;; normalize arbitrary RuntimeException or untyped exceptions to a pass.
    (if (and (keyword? upstream-type)
             (some-> upstream-type namespace (.startsWith "gesso."))
             (keyword? reason))
      (result kind {}
              (blocker :invalid-source :source-check-rejected
                       [:source]
                       {:reason reason :upstream-error-type upstream-type
                        :upstream-at (when (vector? at) at)})
              #{:original-source-validation-failed})
      (throw ex))))

(defn diagnose!
  "Give the single deterministic first blocker for a CURRENT source check.

   Input is exactly {:kind <supported keyword> :source <ORIGINAL input>}; no
   report is accepted as evidence. Upstream source validation is run FIRST.
   Invalid typed sources are reported as blockers; unknown exceptions propagate.
   Every successful source check still has an open assumption / proof boundary,
   so this function *never* returns :certified, an admission token, or success
   authority. Reports are copyable and are not safe as security gates."
  [request]
  (when-not (and (plain-map? request) (nil? (meta request))
                 (= #{:kind :source} (set (keys request)))
                 (contains? kinds (:kind request)))
    (reject! :invalid-request-shape []))
  (let [{:keys [kind source]} request
        original (get original-checkers kind)
        public (get public-checker-vars kind)]
    (when-not (identical? original (var-get public))
      (reject! :substituted-upstream-checker [:kind]))
    (let [evidence (try (original source)
                        (catch clojure.lang.ExceptionInfo ex
                          (source-rejection kind ex)))]
      ;; source-rejection produces a ready diagnostic, never a checker report.
      (if (= diagnostic-type (:type evidence))
        evidence
        (case kind
          :induction (from-induction source evidence)
          :inductive-report (from-inductive-report source evidence)
          :application-induction (from-application source evidence)
          :observed-correspondence (from-observed source evidence)
          :finite-model (from-finite source evidence))))))

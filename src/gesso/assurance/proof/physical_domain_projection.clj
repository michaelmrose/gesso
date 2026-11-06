(ns gesso.assurance.proof.physical-domain-projection
  "v1491: Join the EXISTING v1412 finite protected XTDB document domain to
   v1234 source-derived typed state and invariant obligations.

   Input is original source, not a proof artifact:
     {:obligation-source <v1234 original construction/rule source>
      :protected-domain-source <v1412 original plan/protected-documents source>
      :field-locations
      {<qualified abstract state field>
       {:target [physical-table document-id] :physical-field <keyword>}}}.

   Rederive the source's full typed state projection and the finite document
   declaration. Every abstract state field (including framed fields and all
   invariant readers) must have one distinct location inside the declared
   protection. Entire document replacements require :fields :all as enforced
   by v1412. A caller cannot pass a made-up obligation or domain report.

   This is a DECLARED, FINITE CORRESPONDENCE MAP, NOT observed XTDB values.
   Neither location assertions, exhaustive database tables, hidden writers,
   runtime type translation, transaction-internal prestate, nor the complete
   intended invariant set are authenticated. No certificate or authority is
   issued. Full reports contain potentially sensitive physical identifiers;
   use redacted-summary for logging."
  (:require [clojure.set :as set]
            [gesso.assurance.proof.obligation :as obligations]
            [gesso.assurance.proof.protected-write-domain :as domain]))

(def projection-version 1)
(def projection-type
  :gesso.assurance.proof.physical-domain-projection/finite-declaration)

(def ^:private source-keys
  #{:obligation-source :protected-domain-source :field-locations})
(def ^:private location-keys #{:target :physical-field})
(def ^:private max-fields 128)
(def ^:private original-obligations obligations/derive-obligations!)
(def ^:private original-domain domain/derive-protected-write-domain!)
(def ^:private original-domain-check domain/require-current-protected-write-domain!)
(def ^:private open-boundaries
  #{:source-declares-all-relevant-abstract-state-fields
    :source-declares-all-relevant-business-invariants
    :abstract-field-to-physical-location-semantics
    :physical-value-type-and-nullability-translation
    :complete-physical-document-and-table-coverage
    :transaction-internal-prestate-and-guard-correspondence
    :actual-physical-writes-and-frame-preservation
    :abstract-operation-to-physical-plan-and-issued-executor-binding
    :all-writer-paths-and-deployment-mediation
    :universal-physical-refinement})

(defn- reject! [reason at]
  (throw (ex-info "Invalid declared physical domain projection."
                  {:physical-domain-projection/rejection true
                   :reason reason :at at})))

(defn- plain-map? [x]
  (and (or (instance? clojure.lang.PersistentArrayMap x)
           (instance? clojure.lang.PersistentHashMap x))
       (nil? (meta x))))

(defn- plain-vector? [x]
  (and (instance? clojure.lang.APersistentVector x) (nil? (meta x))))

(defn- exact! [x expected at]
  (when-not (and (plain-map? x) (= expected (set (keys x))))
    (reject! :invalid-map-shape at))
  x)

(defn- original-checkers! []
  ;; Ordinary in-process accidental-substitution protection, not a defense
  ;; against malicious JVM instrumentation or a deployment proof.
  (when-not (and (identical? original-obligations
                             (var-get #'obligations/derive-obligations!))
                 (identical? original-domain
                             (var-get #'domain/derive-protected-write-domain!))
                 (identical? original-domain-check
                             (var-get #'domain/require-current-protected-write-domain!)))
    (reject! :substituted-source-checker [])))

(defn- checked-obligations! [source]
  (let [report (try (original-obligations source)
                    (catch Exception _
                      (reject! :invalid-obligation-source [:obligation-source])))]
    (when-not (and (= obligations/obligation-type (:type report))
                   (= obligations/obligation-version (:version report))
                   (= source (:source report))
                   (= :all-obligations-open (:status report))
                   (= :not-proved (:proof-status report))
                   (= :not-issued (:certificate report))
                   (= :not-admitted (:admission report))
                   (= :none (:authority report))
                   (map? (:invariants report)))
      (reject! :unexpected-obligation-contract [:obligation-source]))
    report))

(defn- checked-domain! [source]
  (let [report (try (let [r (original-domain source)]
                      (original-domain-check source r))
                    (catch Exception _
                      (reject! :invalid-protected-domain-source
                               [:protected-domain-source])))]
    (when-not (and (= domain/domain-type (:type report))
                   (= domain/domain-version (:version report))
                   (= :declared-finite-domain-only (:proof-status report))
                   (= :not-issued (:certificate report))
                   (= :not-admitted (:admission report))
                   (= :none (:authority report))
                   (= :not-established (:writer-path-closure report))
                   (= :not-established (:physical-write-footprint-complete? report)))
      (reject! :unexpected-domain-contract [:protected-domain-source]))
    report))

(defn- location! [field loc protections]
  (exact! loc location-keys [:field-locations field])
  (let [{:keys [target physical-field]} loc]
    (when-not (and (plain-vector? target) (= 2 (count target))
                   (keyword? (first target)) (some? (second target))
                   (not (coll? (second target)))
                   (keyword? physical-field)
                   (not (#{:xt/id :xt/valid-from :xt/valid-to :_id}
                         physical-field)))
      (reject! :unsupported-physical-field-location
               [:field-locations field]))
    (let [declared (get protections target ::missing)]
      (when (= ::missing declared)
        (reject! :physical-target-outside-protection
                 [:field-locations field :target]))
      (when-not (or (= :all declared)
                    (and (set? declared) (contains? declared physical-field)))
        (reject! :physical-field-outside-protection
                 [:field-locations field :physical-field]))))
  loc)

(defn describe-physical-domain-projection!
  "Check all locations of the ABSTRACT typed projection against the v1412
   protected-domain declaration. No arbitrary callbacks or XTDB queries run.

   A complete *declared* mapping is useful for later physical readback/frame
   checking, but cannot itself certify that values came from the claimed
   physical locations or that the enumerated domain is globally exhaustive."
  [source]
  (exact! source source-keys [])
  (original-checkers!)
  (let [ob (checked-obligations! (:obligation-source source))
        dom (checked-domain! (:protected-domain-source source))
        typed-fields (set (keys (get-in source
                                        [:obligation-source :construction :state-types])))
        locs (:field-locations source)
        entries (:protected-documents dom)
        protections (into {} (map (juxt :target :fields) entries))]
    (when-not (and (plain-map? locs) (<= 1 (count locs) max-fields)
                   (= typed-fields (set (keys locs))))
      (reject! :missing-or-extra-abstract-field-location [:field-locations]))
    (doseq [[f loc] locs]
      (location! f loc protections))
    (when-not (= (count locs)
                 (count (set (map (fn [{:keys [target physical-field]}]
                                    [target physical-field])
                                  (vals locs)))))
      (reject! :aliased-abstract-fields [:field-locations]))
    (let [readers (into #{}
                        (mapcat :readers)
                        (vals (:invariants ob)))
          transition (get-in (:obligation-source source) [:construction])
          _ (when-not (set/subset?
                       (set/union readers (:writes transition) (:frame transition))
                       typed-fields)
              (reject! :unmapped-state-rule-field [:obligation-source]))
          mapped-targets (set (map (comp :target val) locs))
          all-protected-targets (set (keys protections))
          frame-targets (set (map :target (:frame-only-protected-documents dom)))
          full-plan? (:declared-replacement-domain-covered? dom)]
      {:type projection-type
       :version projection-version
       :checking :fresh-typed-obligations-and-v1412-finite-protected-domain
       :scope :only-declared-typed-state-fields-and-enumerated-documents
       :source source
       :transition-fingerprint (:transition-fingerprint ob)
       :obligation-fingerprint (:fingerprint ob)
       :declared-abstract-state-fields typed-fields
       :declared-invariant-ids (set (keys (:invariants ob)))
       :declared-invariant-read-fields readers
       :field-locations locs
       :mapped-physical-targets mapped-targets
       :declared-protected-targets all-protected-targets
       :unmapped-protected-targets (set/difference all-protected-targets mapped-targets)
       :frame-only-targets frame-targets
       :declared-state-field-count (count typed-fields)
       :protected-document-count (:protected-document-count dom)
       :declared-command-target-count (:declared-command-target-count dom)
       :declared-command-replacements-covered? (boolean full-plan?)
       :status (if full-plan?
                 :conditional-finite-state-to-protection-location-join
                 :open-command-target-protection)
       :open-obligations open-boundaries
       :physical-location-authenticated? false
       :physical-snapshot-origin :not-established
       :writer-path-closure :not-established
       :complete-physical-domain-projection :not-established
       :physical-frame-preservation :not-established
       :universal-refinement :not-established
       :proof-status :declarations-only
       :certificate :not-issued
       :admission :not-admitted
       :transaction-authority :none
       :authority :none})))

(defn require-current-physical-domain-projection!
  "Recompute from original declarations; reject copied, altered or stale
   reports. Rechecking does not authenticate the deployment or XTDB reads."
  [report source]
  (let [now (describe-physical-domain-projection! source)]
    (when-not (and (plain-map? report) (= report now))
      (reject! :stale-or-substituted-projection [:report]))
    now))

(defn redacted-summary
  "Return diagnostic counts/statuses without field names, document IDs,
   source plans, invariant identifiers, or operation payloads."
  [report]
  (let [r (require-current-physical-domain-projection!
           report (:source report))]
    (select-keys r
                 [:type :version :checking :scope
                  :declared-state-field-count :protected-document-count
                  :declared-command-target-count
                  :declared-command-replacements-covered?
                  :status :open-obligations :physical-location-authenticated?
                  :physical-snapshot-origin :writer-path-closure
                  :complete-physical-domain-projection
                  :physical-frame-preservation :universal-refinement
                  :proof-status :certificate :admission
                  :transaction-authority :authority])))

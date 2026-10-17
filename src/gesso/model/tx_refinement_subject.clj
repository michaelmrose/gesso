(ns gesso.model.tx-refinement-subject
  "Reproducible, *unattested* instance-subject identity for XTDB refinement.

   Derives a digest from an admitted Gesso transaction plan, its selected
   abstract prestate, the current nine-premise ledger, and explicitly supplied
   backend / Gesso / application descriptors. This is a binding of the
   *supplied bytes and declarations*, not proof that those descriptions match
   installed binaries, loaded policy, external writers, or physical execution.

   There is intentionally no proof verification, evidence admission, or premise
   discharge here. In particular, this module does not modify the v1031
   generic evidence reviewer: application-specific evidence review must be
   separately wired to this instance identity and independently authenticated.
   The digest is not a confidentiality primitive: low-entropy state can be
   guessed. Do not publish IDs derived from private application data broadly."
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [gesso.model.command :as command]
            [gesso.model.tx-model :as model]
            [gesso.model.tx-refinement-premises :as premises])
  (:import [java.nio.charset StandardCharsets]
           [java.security MessageDigest]
           [java.time Instant LocalDate LocalDateTime OffsetDateTime ZonedDateTime]
           [java.util Date UUID]))

(def subject-scope
  {:identity :gesso.model.tx/serial-refinement-instance-subject-v1
   :upstream :gesso.model.tx/serial-refinement-premise-ledger-v1
   :checked #{:admitted-plan-and-valid-local-argument
              :deterministic-typed-content-encoding
              :selected-abstract-prestate-included
              :backend-and-source-descriptors-included
              :declared-application-footprint-included
              :canonical-plan-footprint-covered
              :exact-premise-catalog-included}
   :not-established #{:source-or-binary-authentication
                      :correctness-of-supplied-backend-settings
                      :application-policy-or-writer-completeness
                      :physical-prestate-provenance
                      :transaction-internal-snapshot-correspondence
                      :evidence-verification-or-premise-discharge
                      :general-xtdb-refinement
                      :production-admission-or-certification
                      :confidentiality-of-low-entropy-digest-input}})

(def ^:private expected-premise-ids
  #{:xtdb-dml-is-serialized-in-transaction-log-order
    :xtdb-asserts-evaluate-against-current-transaction-state
    :xtdb-assert-failure-rolls-back-all-transaction-writes
    :xtdb-put-docs-atomically-realizes-command-after-documents
    :xtdb-sql-predicates-refine-finite-guard-semantics
    :transaction-valid-time-and-value-representation-is-admitted
    :concrete-transaction-internal-prestate-relates-to-abstract-prestate
    :relevant-external-writers-and-policy-dependencies-are-closed
    :supplied-abstract-prestate-is-complete-and-genuine})

(defn- fail! [reason]
  ;; Never put source values, identifiers, hashes, or policy data in ex-data.
  (throw (ex-info "XTDB refinement instance subject is not admissible."
                  {:error/type ::invalid-subject :error/reason reason})))

(defn- hex-digest? [value]
  (and (string? value) (boolean (re-matches #"[0-9a-f]{64}" value))))

(defn- nonblank? [value]
  (and (string? value) (<= 1 (count value) 256) (not (str/blank? value))))

(defn- distinct-vector? [v element?]
  (and (vector? v) (= (count v) (count (distinct v)))
       (every? element? v)))

(defn- target? [t]
  (and (vector? t) (= 2 (count t)) (keyword? (first t))
       (some? (second t))))

(defn- validate-environment! [environment]
  (let [{:keys [backend gesso application]} environment]
    (when-not (and (map? environment)
                   (= #{:backend :gesso :application} (set (keys environment)))
                   (map? backend)
                   (= #{:product :version :build-sha256 :settings}
                      (set (keys backend)))
                   (= :xtdb (:product backend))
                   (nonblank? (:version backend))
                   (hex-digest? (:build-sha256 backend))
                   (map? (:settings backend))
                   (map? gesso)
                   (= #{:source-sha256} (set (keys gesso)))
                   (hex-digest? (:source-sha256 gesso))
                   (map? application)
                   (= #{:id :operation :source-sha256
                        :dependency-targets :writer-ids :policy-rule-ids}
                      (set (keys application)))
                   (keyword? (:id application))
                   (keyword? (:operation application))
                   (hex-digest? (:source-sha256 application))
                   (distinct-vector? (:dependency-targets application) target?)
                   (distinct-vector? (:writer-ids application) keyword?)
                   (distinct-vector? (:policy-rule-ids application) keyword?))
      (fail! :invalid-environment))
    environment))

;; Canonical serialization is deliberately type tagged. Never hash `pr-str`
;; directly over maps/sets (iteration order is unstable); never stringify an
;; unknown Java object or function (often includes process-local identity).
;; We reject such objects rather than silently mint a non-reproducible ID.
(defn- canonical [value]
  (let [visited (volatile! 0)]
    (letfn [(encode [x depth]
              (vswap! visited inc)
              (when (or (> depth 64) (> @visited 50000))
                (fail! :subject-too-large))
              (cond
                (nil? x) [:nil]
                (true? x) [:boolean true]
                (false? x) [:boolean false]
                (string? x) [:string x]
                (keyword? x) [:keyword (namespace x) (name x)]
                (symbol? x) [:symbol (namespace x) (name x)]
                (char? x) [:character (str x)]
                (instance? UUID x) [:uuid (str x)]
                (instance? Instant x) [:instant (str x)]
                (instance? LocalDate x) [:local-date (str x)]
                (instance? LocalDateTime x) [:local-date-time (str x)]
                (instance? OffsetDateTime x) [:offset-date-time (str x)]
                (instance? ZonedDateTime x) [:zoned-date-time (str x)]
                (instance? java.sql.Timestamp x) [:sql-timestamp (str (.toInstant ^java.sql.Timestamp x))]
                (instance? Date x) [:legacy-date (.getTime ^Date x)]
                (integer? x) [:integer (.getName (class x)) (str x)]
                (ratio? x) [:ratio (str x)]
                (instance? java.math.BigDecimal x) [:decimal (.toPlainString ^java.math.BigDecimal x)]
                (and (number? x) (Double/isFinite (double x)))
                [:floating (.getName (class x)) (str x)]
                (map? x)
                (let [entries (mapv (fn [[k v]] [(encode k (inc depth))
                                                  (encode v (inc depth))]) x)]
                  [:map (vec (sort-by (comp pr-str first) entries))])
                (set? x)
                [:set (vec (sort-by pr-str (map #(encode % (inc depth)) x)))]
                (vector? x)
                [:vector (mapv #(encode % (inc depth)) x)]
                (list? x)
                [:list (mapv #(encode % (inc depth)) x)]
                :else (fail! :unsupported-content-type)))]
      (encode value 0))))

(defn- sha256 [^String data]
  (let [digest (.digest (MessageDigest/getInstance "SHA-256")
                        (.getBytes data StandardCharsets/UTF_8))]
    (apply str (map #(format "%02x" (bit-and 0xff %)) digest))))

(defn derive-subject
  "Build an instance-bound *candidate subject identifier*, without I/O.

   Inputs:
     abstract-state  finite state accepted by tx-model
     plan            ordinary Gesso model transaction in admitted subset
     environment     EXACT descriptor with :backend, :gesso, :application

   Application :dependency-targets must at least include all canonical
   predicate/read targets and all command write targets. Additional named
   policy dependencies and :writer-ids are included in the digest but their
   completeness is NOT established. The descriptors' SHA values are caller
   claims, not independently checked artifacts or proven closure.

   The digest includes selected prestate by design. It names ONE finite
   instance, not a universal operation theorem. No plan, state, SQL, or
   identifying source material appears in the returned report."
  [abstract-state plan environment]
  (validate-environment! environment)
  (model/require-state abstract-state)
  (let [admitted (model/admitted-plan plan)
        ledger (premises/derive-obligation-ledger abstract-state admitted)
        declared (set (get-in environment [:application :dependency-targets]))
        checks (:checks (model/step abstract-state admitted))
        read-targets (set (map :target checks))
        write-targets (set (map command/target (:commands admitted)))
        required (set/union read-targets write-targets)
        ps (:open-premises ledger)
        ids (set (map :id ps))]
    (when-not (and (= expected-premise-ids ids)
                   (= 9 (count ps))
                   (= (:upstream subject-scope) (:obligation ledger))
                   (= :checked (get-in ledger [:local-argument :status]))
                   (= :open-external-premises (:status ledger))
                   (= #{} (:external-premises-discharged ledger))
                   (false? (:general-refinement-proved? ledger))
                   (set/subset? required declared))
      (fail! :incomplete-subject-contract))
    (let [input [(:identity subject-scope)
                 {:premise-catalog {:scope (:scope ledger)
                                    :premises (mapv #(select-keys % [:id :owner :discharge-route :needed :status]) ps)}
                  :abstract-state abstract-state
                  :admitted-plan admitted
                  :backend (:backend environment)
                  :gesso (:gesso environment)
                  :application (-> (:application environment)
                                   (update :dependency-targets #(vec (sort-by (comp pr-str canonical) %)))
                                   (update :writer-ids #(vec (sort-by str %)))
                                   (update :policy-rule-ids #(vec (sort-by str %))))}]
          serial (pr-str (canonical input))]
      (when (> (count (.getBytes serial StandardCharsets/UTF_8)) 2097152)
        (fail! :subject-too-large))
      {:identity (:identity subject-scope)
       :subject-sha256 (sha256 serial)
       :status :bound-declarations-unattested
       :binding-granularity :one-selected-abstract-prestate-and-plan
       :catalog-bound? true
       :plan-and-prestate-bound? true
       :backend-and-source-descriptors-bound? true
       :declared-application-footprint-bound? true
       :canonical-targets-covered? true
       :descriptors-authenticated? false
       :application-dependencies-proved-complete? false
       :application-writers-proved-closed? false
       :physical-prestate-attested? false
       :evidence-verified? false
       :premises-discharged #{}
       :general-refinement-proved? false
       :application-certified? false
       :scope subject-scope})))

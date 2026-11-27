(ns gesso.assurance.reference-source-write-argument
  "Source-relative symbolic mapped replacement argument. No provider theorem."
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [gesso.assurance.reference-mapped-writes :as writes]
            [gesso.assurance.reference-recovery-projection :as projection]
            [gesso.assurance.transition :as construction]
            [gesso.model.certified-plan :as plan]
            [gesso.model.mapped-frame :as frame])
  (:import [java.security MessageDigest]
           [java.util HexFormat]))

(def profile :conditional-source-write-equations-v1)
(def ^:private original-writes writes/derive-transition!)
(def ^:private original-mapping plan/check-target-mapping!)
(def ^:private original-build construction/build-guarded-replacement!)
(def ^:private original-effects frame/check-effects!)
(def ^:private original-catalog projection/check-catalog!)

(def ^:private reviewed-sources
  {"gesso/assurance/reference_mapped_writes.clj" "9343a3d4756ba448ec20aa0be93284f10f5f7890b98a85691980b12478d5c1a3"
   "gesso/model/mapped_frame.clj" "687ca3efd8a975a9b872971fc166a310d11465244f81c485fcd868299564ce8d"
   "gesso/assurance/transition.clj" "be8da836313dbbb3f30f3e63ef273509387ee761a4d4a3f7ab502f6edae9511a"})

(defn- reject! [reason]
  (throw (ex-info "Unqualified source-write argument" {:reason reason})))

(defn- boundaries! []
  (when-not (and (identical? original-writes writes/derive-transition!)
                 (identical? original-mapping plan/check-target-mapping!)
                 (identical? original-build construction/build-guarded-replacement!)
                 (identical? original-effects frame/check-effects!)
                 (identical? original-catalog projection/check-catalog!))
    (reject! :source-write-boundary-replaced))
  (doseq [[path expected] reviewed-sources]
    (let [resource (io/resource path)]
      (when-not (and resource
                     (= expected (with-open [in (io/input-stream resource)]
                                   (.formatHex (HexFormat/of)
                                               (.digest (MessageDigest/getInstance "SHA-256") (.readAllBytes in))))))
        (reject! :unreviewed-source-write-artifact)))))

(defn- address [[kind a b]]
  (if (= :journal-slot kind) [kind a b] [:document a b]))
(defn- column [[_ _ _ k]] (keyword (str/replace (name k) "_" "-")))
(defn- pre [field] [:pre field])

(defn- rhs! [original field]
  (if-not (contains? (:writes original) field)
    (pre field)
    (let [[op value :as expression] (get-in original [:replacements field])]
      (when-not (= 2 (count expression)) (reject! :unsupported-replacement))
      (case op
        :literal [:literal value]
        :read (cond (contains? (:state-types original) value) (pre value)
                    (contains? (:input-types original) value) [:input value]
                    :else (reject! :unknown-replacement-reader))
        (reject! :unsupported-replacement)))))

(defn- document-branch [original entries present?]
  ;; Symbolic execution of the reviewed forward writer's row merge. Extra
  ;; effects cannot touch these keys by the checked effects precondition.
  (let [fields (set (keys entries))
        written (set/intersection fields (:writes original))
        initial (when present? (into {} (map (fn [[f c]] [(column c) (pre f)])) entries))
        absent (when-not present? (into {} (map (fn [f] [(pre f) [:literal nil]])) fields))
        substitute #(walk/postwalk-replace absent %)
        selected (if (seq written) (if present? written fields) #{})
        row (if (seq written)
              (reduce (fn [m f] (assoc m (column (entries f)) (substitute (rhs! original f))))
                      (or initial {}) selected)
              initial)
        actual (into {} (map (fn [[f c]] [f (if row (get row (column c) ::missing) [:literal nil])])) entries)]
    {:presence (if present? :present :absent) :materialized-fields selected :projected actual}))

(defn- check-document! [original entries present?]
  (let [fields (set (keys entries))
        absent (when-not present? (into {} (map (fn [f] [(pre f) [:literal nil]])) fields))
        goal (into {} (map (fn [f] [f (walk/postwalk-replace absent (rhs! original f))])) fields)
        result (document-branch original entries present?)]
    (when-not (= goal (:projected result)) (reject! :source-write-equation-mismatch))
    (assoc result :expected goal)))

(defn- derive-alternative! [original coordinates cases case-alternatives]
  (let [built (original-build original)
        mapping (original-mapping original coordinates)
        id (get-in original [:subject :id])
        groups (group-by (comp address val) coordinates)
        slots (filter (fn [[f c]] (and (= :journal-slot (first c)) (contains? (:writes original) f))) coordinates)]
    (when-not (and (not-any? :optional? (concat (vals (:state-types original)) (vals (:input-types original))))
                   (empty? (set/intersection (set (keys (:state-types original))) (set (keys (:input-types original)))))
                   (= (:replacements original) (get-in built [:transition :assignments]))
                   (= (:guard original) (get-in built [:transition :guard]))
                   (= (:frame original) (:frame built))
                   (= 1 (count slots)))
      (reject! :unsupported-source-write-fragment))
    (let [equations
          (into {} (map (fn [[target pairs]]
                          (let [entries (into {} pairs)]
                            [target
                             (if (= :journal-slot (first target))
                               (let [[field] (first pairs)
                                     written? (contains? (:writes original) field)
                                     keys (filter #(= id (val %)) case-alternatives)
                                     k (ffirst keys)]
                                 (when-not (= 1 (count pairs)) (reject! :aliased-journal-slot))
                                 (if written?
                                   (do (when-not (and (= 1 (count keys)) (= (subvec target 1) (subvec k 0 2))
                                                      (= (rhs! original field) [:literal (get cases k)])
                                                      (keyword? (get cases k)))
                                         (reject! :journal-write-equation-mismatch))
                                       {:required-prestate :absent :case k :projected {field (rhs! original field)}})
                                   {:projected {field (pre field)} :effect :unchanged-slot}))
                               (cond-> [(check-document! original entries true)]
                                 (= :nullable-document (first (val (first pairs))))
                                 (conj (check-document! original entries false))))]))) groups)]
      {:alternative-id id :mapping-fingerprint (:fingerprint mapping)
       :guard (:guard original) :assignments (:replacements original) :frame (:frame original)
       :equations equations :status :conditional-symbolic-write-correspondence})))

(defn derive-catalog!
  "Check the original journal-writing alternatives. For each physical target,
   use symbolic arbitrary prestate/input values and all legal local presence
   branches. Independence of target coordinates allows composition without
   enumerating cross-target values or presence combinations. The original
   guard must hold, bindings must be admitted, the journal slot and ID must be
   fresh and supplemental effects disjoint. Reviewed source correspondence
   and ordinary map operations are assumptions, not compiler attestation."
  [source declarations fingerprint cases]
  (let [base {:profile profile :scope :original-declared-journal-writing-alternatives
              :universal-refinement :not-established :proof-discharge :not-performed
              :authority :none :certificate :not-issued}]
    (try
      (boundaries!)
      (let [checked (original-catalog source declarations fingerprint cases)
            operations (get-in source [:inductive-source :catalog :operations])
            coordinates (:target-map source)
            writers (set (vals (:case-alternatives checked)))]
        (when-not (and (= :matched (:status checked)) (seq writers))
          (reject! :unqualified-source-write-catalog))
        (let [reports (into {} (keep (fn [{:keys [id construction]}]
                                       (when (contains? writers id)
                                         [id (derive-alternative! construction coordinates cases (:case-alternatives checked))]))) operations)]
          (when-not (= writers (set (keys reports))) (reject! :incomplete-source-write-coverage))
          (boundaries!)
          (assoc base :status :conditional-symbolic-catalog-derivation
                 :catalog-fingerprint fingerprint :reviewed-sources reviewed-sources
                 :alternatives reports :outside-fragment (set/difference (set (map :id operations)) writers)
                 :conclusion :projected-put-image-satisfies-original-replacement-and-frame-equations
                 :remaining-conditions #{:original-typed-guard-holds-and-inputs-admitted
                                         :faithful-closed-prestate-and-coordinate-projection
                                         :fresh-journal-slot-and-command-id
                                         :faithful-declared-outcome-and-record-construction
                                         :disjoint-supplemental-effects-and-physical-identifiers
                                         :faithful-reviewed-source-map-semantics-and-loaded-code
                                         :faithful-original-abstract-transition-semantics})))
      (catch clojure.lang.ExceptionInfo e
        (assoc base :status :not-established :reason (or (:reason (ex-data e)) :unqualified-source-write-argument))))))

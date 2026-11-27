(ns gesso.assurance.reference-recovery-expression
  "Conditional expression refinement for the exact original selected catalog.
   Descriptive evidence only; neither a certificate nor a premise discharge."
  (:require [gesso.assurance.proof.literal-evaluator-refinement :as refinement]
            [gesso.assurance.proof.operation-set :as operation-set]))

(def profile :original-catalog-expression-refinement-v1)
(def binding-profile :selected-expression-premise-refinement-v1)
(def ^:private original-refinement refinement/derive!)
(def ^:private original-catalog operation-set/check-operation-set!)

(defn boundaries-current? []
  (and (identical? original-refinement refinement/derive!)
       (identical? original-catalog operation-set/check-operation-set!)))

(defn- qualified? [rule types report]
  (and (= refinement/profile (:profile report))
       (= :conditional-source-refinement (:status report))
       (= rule (:rule report))
       (= (select-keys types (:readers rule)) (:reader-types report))
       (= :reviewed-interpreter-truth-factors-through-abstract-cells (:conclusion report))
       (= :matched-captured-var-identities (get-in report [:runtime-identity :status]))
       (= :conditional-source-handoff (get-in report [:admission-source :status]))
       (set? (:conditions report)) (seq (:conditions report))
       (= :not-established (:loaded-code-correspondence report) (:universal-evaluator-correspondence report))
       (= :not-performed (:proof-discharge report))
       (= :none (:authority report)) (= :not-issued (:certificate report))))

(defn check-catalog!
  "Recheck exact original operation-set identity and every original rule,
   including Init. An unsupported rule leaves the catalog result unestablished."
  [catalog expected-fingerprint]
  (let [base {:profile profile :scope :original-declared-catalog-only
              :universal-evaluator-correspondence :not-established
              :proof-discharge :not-performed :authority :none :certificate :not-issued}
        fail #(assoc base :status :not-established :reason %)]
    (try
      (if-not (boundaries-current?)
        (fail :expression-refinement-boundary-replaced)
        (let [checked (original-catalog catalog)
              types (get-in catalog [:operations 0 :construction :state-types])
              rules (:invariants catalog)]
          (if-not (and (string? expected-fingerprint)
                       (= expected-fingerprint (:fingerprint checked))
                       (= operation-set/report-type (:type checked))
                       (= (:id catalog) (:catalog-id checked))
                       (= (set (keys rules)) (:invariant-ids checked))
                       (seq rules) (map? types))
            (fail :expression-catalog-mismatch)
            (let [init (original-refinement (:initial-condition catalog) types)
                  reports (into {} (map (fn [[id rule]] [id (original-refinement rule types)])) rules)
                  all (cons init (vals reports))]
              (if (and (boundaries-current?)
                       (qualified? (:initial-condition catalog) types init)
                       (every? (fn [[id rule]] (qualified? rule types (reports id))) rules)
                       (apply = (map :conditions all))
                       (apply = (map :source-artifacts all)))
                (assoc base :status :conditional-catalog-refinement
                       :catalog-id (:id catalog) :catalog-fingerprint expected-fingerprint
                       :invariant-ids (set (keys rules)) :initial-condition init :invariants reports
                       :conditions (:conditions init)
                       :cases-checked (reduce + (map :cases-checked all)))
                (fail :unqualified-catalog-expression-refinement))))))
      (catch clojure.lang.ExceptionInfo e
        (fail (or (:reason (ex-data e)) :unsupported-catalog-expression-refinement))))))

(defn bind-premise
  "Bind retained original-cut evidence to the already bound transfer premise.
   Internal only: the caller supplies an issued registry entry and freshly
   composed inventory, never a caller-provided evidence map."
  [checked transfer inventory abstract]
  (let [base {:profile binding-profile :scope :original-selected-catalog-expression-only
              :premise-acceptance :not-performed :proof-discharge :not-performed
              :universal-evaluator-correspondence :not-established
              :authority :none :certificate :not-issued}
        evidence (:catalog-expression-refinement checked)
        subject (:judgment-subject checked)
        fp (get-in transfer [:condition-premise-fingerprints :faithful-invariant-expression-semantics])
        premise (get-in inventory [:premises-by-fingerprint fp])]
    (if (and (= :checked (:status checked)) (= :bound (:status transfer))
             (= profile (:profile evidence)) (= :conditional-catalog-refinement (:status evidence))
             (= :not-performed (:proof-discharge evidence) (:proof-discharge transfer))
             (= :not-established (:universal-evaluator-correspondence evidence))
             (= :none (:authority evidence)) (= :not-issued (:certificate evidence))
             (string? (:recovery-cut-ref checked)) (= (:recovery-cut-ref checked) (:recovery-cut-ref transfer))
             (string? (:catalog-fingerprint checked))
             (= (:catalog-fingerprint checked) (:catalog-fingerprint subject) (:catalog-fingerprint evidence)
                (get-in abstract [:identity :catalog-fingerprint]))
             (= (:catalog-id subject) (:catalog-id evidence) (:catalog-id abstract))
             (= (:abstract-induction-fingerprint subject) (:abstract-report-fingerprint transfer)
                (:fingerprint abstract) (get-in inventory [:abstract :report-fingerprint]))
             (= (:invariant-ids checked) (:invariant-ids evidence) (:invariant-ids transfer)
                (:declared-invariant-ids abstract) (set (keys (:invariants evidence))))
             (= :matched (get-in checked [:expression-correspondence :status]))
             (= (:invariant-ids checked) (get-in checked [:expression-correspondence :invariant-ids]))
             (seq (:conditions evidence)) (string? fp)
             (contains? (:required-open-premise-fingerprints transfer) fp)
             (contains? (:unaccepted-premise-fingerprints inventory) fp)
             (= {:id :reference-physical/invariant-expression-semantics :family :physical
                 :dependency-fingerprints #{} :acceptance :not-performed :discharge-state :open} premise))
      (assoc base :status :conditional-premise-support
             :premise-fingerprint fp :recovery-cut-ref (:recovery-cut-ref checked)
             :catalog-fingerprint (:catalog-fingerprint evidence)
             :invariant-ids (:invariant-ids evidence) :includes-initial-condition true
             :remaining-conditions (:conditions evidence) :premise-state :open)
      (assoc base :status :not-established :reason :unqualified-original-expression-premise))))

(ns gesso.assurance.reference-transaction
  "Independent, deliberately small execution semantics for a selected
   current-state transaction fragment. No XTDB, decoder, constructor or
   provider outcome is consulted. Not a temporal model or a proof of XTDB."
  (:require [clojure.string :as str]))

(def profile :selected-current-state-transaction-v4)

(defn- reject! [reason]
  (throw (ex-info "Unsupported reference transaction." {::unsupported true :reason reason})))

(defn- table? [x]
  (and (simple-keyword? x) (re-matches #"[a-z][a-z0-9_]*" (name x))
       (not (str/starts-with? (name x) "xt_"))))

(defn- id? [x]
  (and (string? x) (not (str/blank? x)) (<= (count x) 1024)))

(defn- scalar? [x]
  (or (and (string? x) (<= (count x) 4096)) (boolean? x)
      (and (integer? x) (<= Long/MIN_VALUE x Long/MAX_VALUE))))

(defn- equality-domain [v]
  ;; Homogeneous closed domains only; no SQL coercion, null or composites.
  (cond
    (instance? Long v) :i64
    (boolean? v) :boolean
    (and (string? v) (<= (count v) 4096)
         (.canEncode (.newEncoder java.nio.charset.StandardCharsets/UTF_8)
                     ^CharSequence v)) :string
    (and (keyword? v) (<= (count (str v)) 4096)
         (re-matches #"[a-zA-Z][a-zA-Z0-9_-]*" (name v))
         (or (nil? (namespace v))
             (re-matches #"[a-zA-Z][a-zA-Z0-9_.-]*" (namespace v)))) :keyword
    (uuid? v) :uuid))

(defn- equality-kind! [op]
  (let [[_ left right] op
        domain (equality-domain left)]
    (when-not (and (= 3 (count op)) domain (= domain (equality-domain right)))
      (reject! :unsupported-reference-equality-operands))
    (if (= left right) :assert-true :assert-false)))

(defn- document? [doc]
  (and (map? doc) (nil? (meta doc)) (id? (:xt/id doc))
       (<= (count doc) 256)
       (every? (fn [[k v]]
                 (or (= :xt/id k)
                     (and (simple-keyword? k) (re-matches #"[a-z][a-z0-9]*" (name k))
                          (scalar? v)))) doc)))

(defn- check-state! [state]
  (when-not (and (map? state) (nil? (meta state)) (<= (count state) 256)
                 (every? (fn [[table rows]]
                           (and (table? table) (map? rows) (nil? (meta rows))
                                (seq rows) (<= (count rows) 16384)
                                (every? (fn [[id doc]]
                                          (and (document? doc) (= id (:xt/id doc)))) rows))) state))
    (reject! :unsupported-reference-prestate)))

;; Exact selected row-read assertion, not a general SQL parser. Quoted
;; identifiers avoid reserved-word ambiguity and have no escape syntax here.
(def ^:private read-pattern
  #"ASSERT EXISTS \(SELECT 1 FROM \"([a-z][a-z0-9_]*)\" WHERE _id = \? AND \"([a-z][a-z0-9]*)\" = \?\)")

(defn- read-assertion! [op]
  (let [[sql id expected] op
        [_ table field] (re-matches read-pattern sql)
        domain (equality-domain expected)]
    (when-not (and (= 3 (count op)) table (table? (keyword table))
                   (id? id) (= :string (equality-domain id))
                   (#{:i64 :boolean :string} domain))
      (reject! :unsupported-reference-read-assertion))
    {:kind :assert-read :table (keyword table) :id id
     :field (keyword field) :expected expected :domain domain}))

(defn- read-matches! [working {:keys [table id field expected domain]}]
  (let [row (get-in working [table id])]
    (if (contains? row field)
      (let [actual (get row field)]
        ;; Do not infer SQL coercion rules from Clojure equality.
        (when-not (= domain (equality-domain actual))
          (reject! :unsupported-reference-read-domain))
        (= expected actual))
      false)))

;; Selected physical count guards: either quoted simple identifiers or
;; unquoted Gesso-prefixed tables, avoiding SQL reserved-word ambiguity.
;; Literal counts occur in infrastructure guards; bound counts are used
;; by the model transaction formatter. No arbitrary WHERE predicates.
(def ^:private count-pattern
  #"ASSERT (\?|0|[1-9][0-9]{0,4}) = \(SELECT COUNT\(\*\) FROM (\"[a-z][a-z0-9_]*\"|gesso_[a-z0-9_]+)( WHERE _id = \?)?\)")

(def ^:private field-count-pattern
  #"ASSERT (\?|[1-9][0-9]{0,4}) = \(SELECT LENGTH\(NEST_ONE \(SELECT \* FROM (\"[a-z][a-z0-9_]*\"|gesso_[a-z0-9_]+) WHERE _id = \?\)\)\)")

(defn- count-assertion! [op kind [_ count-token table id-clause]]
  (let [bound? (= "?" count-token)
        id-required? (or (= :assert-field-count kind) (some? id-clause))
        args (subvec op 1)
        expected (if bound? (first args) (Long/parseLong count-token))
        id-args (if (and bound? (seq args)) (subvec args 1) args)
        id (first id-args)
        table (keyword (str/replace table "\"" ""))]
    (when-not (and (table? table) (instance? Long expected)
                   (if (= :assert-field-count kind)
                     (<= 1 expected 8192)
                     (<= 0 expected 16384))
                   (= (count args) (+ (if bound? 1 0) (if id-required? 1 0)))
                   (or (not id-required?)
                       (and (id? id) (= :string (equality-domain id)))))
      (reject! :unsupported-reference-count-assertion))
    {:kind kind :table table :id id :expected expected}))

(defn- count-matches? [working {:keys [kind table id expected]}]
  (let [rows (get working table)]
    (case kind
      :assert-row-count (= expected (if id (if (contains? rows id) 1 0) (count rows)))
      :assert-field-count (when-let [doc (get rows id)]
                            (= expected (count doc))))))

(defn- check-op! [op]
  (when-not (and (vector? op) (nil? (meta op)) (seq op))
    (reject! :unsupported-reference-operation))
  (cond
    (= ["ASSERT TRUE"] op) :assert-true
    (= ["ASSERT FALSE"] op) :assert-false
    (= "ASSERT ? = ?" (first op)) (equality-kind! op)
    (and (string? (first op)) (re-matches read-pattern (first op)))
    (read-assertion! op)
    (and (string? (first op)) (re-matches count-pattern (first op)))
    (count-assertion! op :assert-row-count (re-matches count-pattern (first op)))
    (and (string? (first op)) (re-matches field-count-pattern (first op)))
    (count-assertion! op :assert-field-count (re-matches field-count-pattern (first op)))
    (= :put-docs (first op))
    (let [docs (subvec op (min 2 (count op)))]
      (when-not (and (table? (second op)) (<= 1 (count docs) 16384)
                     (every? document? docs)
                     (= (count docs) (count (distinct (map :xt/id docs)))))
        (reject! :unsupported-reference-put))
      :put)
    :else (reject! :unsupported-reference-operation)))

(defn execute
  "Interpret a complete, supported operation vector on a canonical map
   {table {string-id document}}. Puts replace entire documents in order;
   literal, homogeneous parameter equality, selected row-read and count
   assertions can abort and return the original state. Reads see the tentative state
   produced by preceding operations. The full vector is structurally validated
   before evaluation, including operations after a false guard; reached reads
   also check the stored operand domain.
   Unsupported input returns no predicted state or outcome. No arbitrary
   SQL, temporal bounds, callbacks, or observed outcomes are accepted."
  [state ops]
  (let [report {:profile profile :authority :none :certificate :not-issued}]
    (try
      (check-state! state)
      (when-not (and (vector? ops) (nil? (meta ops)) (<= 1 (count ops) 4096))
        (reject! :unsupported-reference-operation-sequence))
      (let [kinds (mapv check-op! ops)]
        (loop [working state i 0]
          (if (= i (count ops))
            (assoc report :status :predicted :committed? true :state working)
            (let [kind (nth kinds i)]
              (case (if (map? kind) (:kind kind) kind)
                :assert-true (recur working (inc i))
                :assert-false (assoc report :status :predicted :committed? false :state state :failed-assertion-index i)
                :assert-read (if (read-matches! working kind)
                               (recur working (inc i))
                               (assoc report :status :predicted :committed? false
                                      :state state :failed-assertion-index i))
                (:assert-row-count :assert-field-count)
                (if (count-matches? working kind)
                  (recur working (inc i))
                  (assoc report :status :predicted :committed? false
                         :state state :failed-assertion-index i))
                :put (let [[_ table & docs] (nth ops i)]
                       (recur (reduce (fn [s doc] (assoc-in s [table (:xt/id doc)] doc)) working docs)
                              (inc i))))))))
      (catch clojure.lang.ExceptionInfo e
        (if (::unsupported (ex-data e))
          (assoc report :status :unsupported :reason (:reason (ex-data e)))
          (throw e))))))

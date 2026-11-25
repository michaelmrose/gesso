(ns gesso.assurance.reference-transaction
  "Independent, deliberately small execution semantics for a selected
   current-state transaction fragment. No XTDB, decoder, constructor or
   provider outcome is consulted. Not a temporal model or a proof of XTDB."
  (:require [clojure.string :as str])
  (:import (java.time Instant ZoneId ZonedDateTime)))

(def profile :selected-current-state-transaction-v13)

(defn- reject! [reason]
  (throw (ex-info "Unsupported reference transaction." {::unsupported true :reason reason})))

(defn- table? [x]
  (and (simple-keyword? x) (re-matches #"[a-z][a-z0-9_]*" (name x))
       (not (str/starts-with? (name x) "xt_"))))

(defn- id? [x]
  (and (string? x) (not (str/blank? x)) (<= (count x) 1024)))

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

(defn- scalar? [x]
  (#{:i64 :boolean :string :keyword} (equality-domain x)))

(defn- value-key? [k]
  ;; Conservative canonical keys, including dotted journal namespaces.
  (and (keyword? k) (re-matches #"[a-z][a-z0-9]*(?:-[a-z0-9]+)*" (name k))
       (or (nil? (namespace k))
           (and (not= "xt" (namespace k))
                (re-matches #"[a-z][a-z0-9]*(\.[a-z][a-z0-9]*)*" (namespace k))))))

(defn- leaf? [v]
  (or (scalar? v) (uuid? v)))

(defn- document-value? [v depth budget]
  ;; Bound recursive validation; accept maps and scalar vectors only.
  (and (<= depth 8) (not (neg? (vswap! budget dec)))
       (cond
         (leaf? v) true
         (map? v) (and (nil? (meta v)) (<= (count v) 256)
                       (every? (fn [[k child]]
                                 (and (value-key? k)
                                      (document-value? child (inc depth) budget))) v))
         (vector? v) (and (nil? (meta v)) (<= (count v) 256)
                          (every? #(and (leaf? %)
                                        (document-value? % (inc depth) budget)) v))
         :else false)))

(def ^:private utc (ZoneId/of "UTC"))
(def ^:private lease-key :gesso.live.outbox/lease-until)

(defn- lease-timestamp? [x]
  (let [i (cond
            (instance? Instant x) x
            (and (instance? ZonedDateTime x) (= utc (.getZone ^ZonedDateTime x)))
            (.toInstant ^ZonedDateTime x))]
    (and i (zero? (mod (.getNano ^Instant i) 1000))
         (not (.isBefore ^Instant i (Instant/parse "0001-01-01T00:00:00Z")))
         (.isBefore ^Instant i (Instant/parse "9999-01-01T00:00:00Z")))))

(defn- normalize-document [table doc]
  ;; The selected provider decoder exposes Instant writes as UTC ZonedDateTime.
  ;; Keep this storage conversion separate from the scalar guard domains.
  (if (and (= :gesso_live_outbox table) (instance? Instant (get doc lease-key)))
    (update doc lease-key #(.atZone ^Instant % utc))
    doc))

(defn- document? [table doc]
  (and (map? doc) (nil? (meta doc))
       (or (uuid? (:xt/id doc))
           (and (id? (:xt/id doc)) (= :string (equality-domain (:xt/id doc)))))
       (<= (count doc) 256)
       (let [budget (volatile! 8192)]
         (every? (fn [[k v]]
                   (or (= :xt/id k)
                       (and (value-key? k)
                            (if (and (= :gesso_live_outbox table) (= lease-key k))
                              (lease-timestamp? v)
                              (document-value? v 0 budget))))) doc))))

(defn- check-state! [state]
  (when-not (and (map? state) (nil? (meta state)) (<= (count state) 256)
                 (every? (fn [[table rows]]
                           (and (table? table) (map? rows) (nil? (meta rows))
                                (seq rows) (<= (count rows) 16384)
                                (every? (fn [[id doc]]
                                          (and (document? table doc) (= id (:xt/id doc))
                                               (= doc (normalize-document table doc)))) rows))) state))
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

(defn- string-id-row! [rows id]
  ;; Existing string-id selectors have no qualified UUID coercion rule.
  (when-not (every? string? (keys rows))
    (reject! :unsupported-reference-id-domain))
  (get rows id))

(defn- read-matches! [working {:keys [table id field expected domain]}]
  (let [row (string-id-row! (get working table) id)]
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
    (when-not (and (table? table) (or (instance? Long expected) (instance? Integer expected))
                   (if (= :assert-field-count kind)
                     (<= 1 expected 8192)
                     (<= 0 expected 16384))
                   (= (count args) (+ (if bound? 1 0) (if id-required? 1 0)))
                   (or (not id-required?)
                       (and (id? id) (= :string (equality-domain id)))))
      (reject! :unsupported-reference-count-assertion))
    {:kind kind :table table :id id :expected expected}))

(def ^:private publication-absence-sql
  "ASSERT 0 = (SELECT COUNT(*) FROM gesso_live_outbox WHERE _id = ?)")

(defn- publication-absence! [op]
  (when-not (and (= 2 (count op)) (uuid? (second op)))
    (reject! :unsupported-reference-publication-identity))
  {:kind :assert-publication-absence :id (second op)})

(defn- publication-absent? [working id]
  (let [rows (get working :gesso_live_outbox)]
    (when-not (every? uuid? (keys rows))
      (reject! :unsupported-reference-publication-id-domain))
    (not (contains? rows id))))

(def ^:private source-absence-sql
  "ASSERT 0 = (SELECT COUNT(*) FROM gesso_live_outbox WHERE gesso$live$outbox$source_id = ?)")

(defn- source-absence! [op]
  (let [source (second op)]
    (when-not (and (= 2 (count op)) (id? source) (= :string (equality-domain source)))
      (reject! :unsupported-reference-publication-source))
    {:kind :assert-source-absence :source source}))

(defn- source-absent! [working source]
  ;; Missing fields are SQL null and do not match. Validate every present
  ;; operand before counting; do not assume unlike-type SQL coercion.
  (let [sources (mapv :gesso.live.outbox/source-id (vals (get working :gesso_live_outbox)))]
    (when-not (every? #(or (nil? %) (= :string (equality-domain %))) sources)
      (reject! :unsupported-reference-publication-source-domain))
    (not-any? #(= source %) sources)))

(defn- count-matches? [working {:keys [kind table id expected]}]
  (let [rows (get working table)]
    (case kind
      :assert-row-count (= expected (if id (if (string-id-row! rows id) 1 0) (count rows)))
      :assert-field-count (when-let [doc (string-id-row! rows id)]
                            (= expected (count doc))))))

;; Exact selected mapped scalar guard; each field contributes three
;; comparisons, with identical bound operands. Do not model STR generally.
(def ^:private mapped-pattern
  #"ASSERT \? = \(SELECT COUNT\(\*\) FROM (gesso_[a-z0-9_]+) WHERE (.+)\)")

(def ^:private mapped-column-pattern
  #"\(([a-z][a-z0-9]*|_id) = \?\)")

(defn- mapped-assertion! [op [_ table predicate]]
  (let [clauses (str/split predicate #" AND " -1)
        args (subvec op 1)]
    (when-not (and (<= 3 (count clauses) (* 3 256))
                   (zero? (mod (count clauses) 3))
                   (= (inc (count clauses)) (count args))
                   (instance? Long (first args)) (= 1 (first args)))
      (reject! :unsupported-reference-mapped-assertion))
    (let [checks
          (mapv (fn [[ordinary printed array-printed] [a b c]]
                  (let [[_ col] (re-matches mapped-column-pattern ordinary)]
                    (when-not (and col
                                   (= printed (str "(STR(" col ") = STR(?))"))
                                   (= array-printed (str "(STR(ARRAY[" col "]) = STR(ARRAY[?]))"))
                                   (scalar? a) (= (equality-domain a) (equality-domain b)
                                                  (equality-domain c))
                                   (= a b c))
                      (reject! :unsupported-reference-mapped-assertion))
                    [(if (= "_id" col) :xt/id (keyword col)) a]))
                (partition 3 clauses) (partition 3 (subvec args 1)))
          [id-field id] (first checks)]
      (when-not (and (= :xt/id id-field) (id? id)
                     (= :string (equality-domain id))
                     (= (count checks) (count (distinct (map first checks)))))
        (reject! :unsupported-reference-mapped-assertion))
      {:kind :assert-mapped :table (keyword table) :id id :checks checks})))

(defn- mapped-matches! [working {:keys [table id checks]}]
  (let [row (string-id-row! (get working table) id)]
    (and row
         ;; Validate every reached present comparison, without relying on
         ;; SQL evaluation order or short-circuiting the field checks.
         (every? true?
                 (mapv (fn [[field expected]]
                         (if-not (contains? row field)
                           false
                           (let [actual (get row field)
                                 a (equality-domain actual)
                                 b (equality-domain expected)]
                             (cond
                               (= a b) (= actual expected)
                               (= #{a b} #{:string :keyword}) false
                               :else (reject! :unsupported-reference-mapped-domain)))))
                       checks)))))

;; Exact slot selectors emitted by command resolution and mapped framing.
;; The slot is keyed by nested principal/operation, not command row identity.
(def ^:private journal-slot-pattern
  #"ASSERT ([01]) = \(SELECT COUNT\(\*\) FROM gesso_commands WHERE \(gesso\$command\$binding\)\.principal\.gesso\$choreo\$identity\$value = \? AND \(gesso\$command\$binding\)\.operation = \?\)")

(defn- journal-slot-assertion! [op [_ n]]
  (let [[_ principal operation] op]
    (when-not (and (= 3 (count op)) (id? principal)
                   (= :string (equality-domain principal))
                   (= :keyword (equality-domain operation)))
      (reject! :unsupported-reference-journal-slot))
    {:kind :assert-journal-slot :principal principal :operation operation
     :expected (Long/parseLong n)}))

(defn- journal-path! [row path]
  (reduce (fn [v field]
            (cond
              (nil? v) nil
              (map? v) (get v field)
              :else (reject! :unsupported-reference-journal-path)))
          row path))

(defn- journal-slot-matches? [working {:keys [principal operation expected]}]
  (let [matches
        (mapv (fn [row]
                (let [p (journal-path! row [:gesso.command/binding :principal
                                            :gesso.choreo.identity/value])
                      o (journal-path! row [:gesso.command/binding :operation])]
                  ;; The selector uses ordinary SQL equality. Unlike
                  ;; non-null types may be coerced; do not guess.
                  (when-not (and (or (nil? p) (= :string (equality-domain p)))
                                 (or (nil? o) (= :keyword (equality-domain o))))
                    (reject! :unsupported-reference-journal-slot-domain))
                  (and (= principal p) (= operation o))))
              (vals (get working :gesso_commands)))]
    (= expected (count (filter true? matches)))))

;; Selected occupied journal guard: recursive maps, scalar leaves and
;; whole scalar vectors with complete indexed comparison triples.
(def ^:private journal-value-pattern
  #"ASSERT 1 = \(SELECT COUNT\(\*\) FROM gesso_commands WHERE \(gesso\$command\$binding\)\.principal\.gesso\$choreo\$identity\$value = \? AND \(gesso\$command\$binding\)\.operation = \? AND (.+)\)")

(defn- journal-column! [column]
  (if (= "_id" column)
    :xt/id
    (let [parts (str/split column #"\$" -1)
          k (if (= 1 (count parts))
              (keyword (str/replace column "_" "-"))
              (keyword (str/join "." (butlast parts))
                       (str/replace (last parts) "_" "-")))]
      (when-not (and (value-key? k)
                     (every? #(re-matches #"[a-z][a-z0-9]*" %) (butlast parts)))
        (reject! :unsupported-reference-journal-column))
      k)))

(defn- journal-guard-path! [path]
  (loop [s path tail ()]
    (when (> (count tail) 8) (reject! :unsupported-reference-journal-path))
    (if-let [[_ col] (re-matches #"\"([a-z_][a-z0-9_$]*)\"" s)]
      (let [root (journal-column! col)]
        (when (some #{:xt/id} tail) (reject! :unsupported-reference-journal-path))
        (when (and (= :xt/id root) (seq tail)) (reject! :unsupported-reference-journal-path))
        (into [root] tail))
      (if-let [[_ parent col] (re-matches #"\((.+)\)\.\"([a-z_][a-z0-9_$]*)\"" s)]
        (recur parent (conj tail (journal-column! col)))
        (reject! :unsupported-reference-journal-path)))))

(defn- scalar-triple? [pairs path v]
  (and (leaf? v)
       (= [(str path " = ?") v] (first pairs))
       (= [(str "STR(" path ") = STR(?)") v] (second pairs))
       (= [(str "STR(ARRAY[" path "]) = STR(ARRAY[?])") v]
          (nth pairs 2 nil))
       (= (equality-domain v) (equality-domain (second (first pairs)))
          (equality-domain (second (second pairs)))
          (equality-domain (second (nth pairs 2 nil))))))

(defn- journal-vector-tail! [pairs path values]
  ;; Require whole equality AND every one-based element triple, in order.
  (when-not (and (nil? (meta values)) (<= (count values) 256)
                 (every? leaf? values))
    (reject! :unsupported-reference-journal-vector))
  (reduce (fn [remaining [i v]]
            (when-not (scalar-triple? remaining (str "(" path ")[" (inc i) "]") v)
              (reject! :unsupported-reference-journal-vector-element))
            (nthnext remaining 3))
          (next pairs) (map-indexed vector values)))

(defn- journal-checks! [clauses args]
  (loop [pairs (seq (map vector clauses args)) checks []]
    (if-let [[clause v] (first pairs)]
      (if-let [[_ path] (re-matches #"LENGTH\((.+)\) = \?" clause)]
        (let [k (journal-guard-path! path)]
          (when-not (and (or (instance? Long v) (instance? Integer v)) (<= 0 v 256))
            (reject! :unsupported-reference-journal-length))
          (if (zero? v)
            (do (when-not (= [(str "STR(" path ") LIKE ?") "{%"] (second pairs))
                  (reject! :unsupported-reference-journal-empty-map))
                (recur (nnext pairs) (conj checks [:map-count k v])))
            (recur (next pairs) (conj checks [:map-count k v]))))
        (let [[_ path] (re-matches #"(.+) = \?" clause)]
          (when-not path (reject! :unsupported-reference-journal-leaf))
          (if (vector? v)
            (recur (journal-vector-tail! pairs path v)
                   (conj checks [:vector (journal-guard-path! path) v]))
            (do (when-not (scalar-triple? pairs path v)
                  (reject! :unsupported-reference-journal-leaf))
                (recur (nthnext pairs 3)
                       (conj checks [:scalar (journal-guard-path! path) v]))))))
      checks)))

(defn- journal-value-assertion! [op [_ predicate]]
  (let [[_ principal operation & args] op
        clauses (str/split predicate #" AND " -1)]
    (when-not (and (id? principal) (= :string (equality-domain principal))
                   (= :keyword (equality-domain operation))
                   (<= 1 (count clauses) 8192) (= (count clauses) (count args)))
      (reject! :unsupported-reference-journal-value-assertion))
    {:kind :assert-journal-value :principal principal :operation operation
     :checks (journal-checks! clauses args)}))

(defn- journal-vector-matches! [actual expected]
  (when-not (vector? actual)
    (reject! :unsupported-reference-journal-vector-domain))
  ;; Check overlapping elements even when lengths differ. Only the
  ;; tested keyword/string distinction may cross scalar domains.
  (let [matches (mapv (fn [a b]
                        (let [ad (equality-domain a) bd (equality-domain b)]
                          (cond
                            (= ad bd) (= a b)
                            (= #{ad bd} #{:string :keyword}) false
                            :else (reject! :unsupported-reference-journal-vector-domain))))
                      actual expected)]
    (and (= (count actual) (count expected)) (every? true? matches))))

(defn- journal-value-matches! [working {:keys [principal operation checks]}]
  (let [matching (filterv (fn [row]
                            (= true (journal-slot-matches? {:gesso_commands {(:xt/id row) row}}
                                                           {:principal principal :operation operation
                                                            :expected 1})))
                          (vals (get working :gesso_commands)))]
    (= 1 (count (filter true?
                        (mapv (fn [row]
                                (every? true?
                                        (mapv (fn [[kind path expected]]
                                                (let [actual (journal-path! row path)]
                                                  (cond
                                                    (nil? actual) false
                                                    (= :map-count kind)
                                                    (if (map? actual)
                                                      (= expected (count actual))
                                                      (reject! :unsupported-reference-journal-map-domain))
                                                    (= :vector kind)
                                                    (journal-vector-matches! actual expected)
                                                    (= (equality-domain actual) (equality-domain expected))
                                                    (= actual expected)
                                                    (= #{(equality-domain actual) (equality-domain expected)}
                                                       #{:string :keyword}) false
                                                    :else (reject! :unsupported-reference-journal-leaf-domain))))
                                              checks))) matching))))))

(defn- check-op! [op]
  (when-not (and (vector? op) (nil? (meta op)) (seq op))
    (reject! :unsupported-reference-operation))
  (cond
    (= ["ASSERT TRUE"] op) :assert-true
    (= ["ASSERT FALSE"] op) :assert-false
    (= "ASSERT ? = ?" (first op)) (equality-kind! op)
    (and (string? (first op)) (re-matches read-pattern (first op)))
    (read-assertion! op)
    (= publication-absence-sql (first op)) (publication-absence! op)
    (= source-absence-sql (first op)) (source-absence! op)
    (and (string? (first op)) (re-matches count-pattern (first op)))
    (count-assertion! op :assert-row-count (re-matches count-pattern (first op)))
    (and (string? (first op)) (re-matches field-count-pattern (first op)))
    (count-assertion! op :assert-field-count (re-matches field-count-pattern (first op)))
    (and (string? (first op)) (re-matches mapped-pattern (first op)))
    (mapped-assertion! op (re-matches mapped-pattern (first op)))
    (and (string? (first op)) (re-matches journal-slot-pattern (first op)))
    (journal-slot-assertion! op (re-matches journal-slot-pattern (first op)))
    (and (string? (first op)) (re-matches journal-value-pattern (first op)))
    (journal-value-assertion! op (re-matches journal-value-pattern (first op)))
    (= :put-docs (first op))
    (let [docs (subvec op (min 2 (count op)))]
      (when-not (and (table? (second op)) (<= 1 (count docs) 16384)
                     (every? #(document? (second op) %) docs)
                     (= (count docs) (count (distinct (map :xt/id docs)))))
        (reject! :unsupported-reference-put))
      :put)
    :else (reject! :unsupported-reference-operation)))

(defn operation-tables
  "Return the table dependency set of one structurally admitted operation.
   Empty means state-independent; nil means unsupported. Use the same
   checked grammar as execution, including infrastructure assertions."
  [op]
  (try
    (let [kind (check-op! op)]
      (case (if (map? kind) (:kind kind) kind)
        (:assert-true :assert-false) #{}
        :put #{(second op)}
        (:assert-read :assert-row-count :assert-field-count :assert-mapped) #{(:table kind)}
        (:assert-journal-slot :assert-journal-value) #{:gesso_commands}
        (:assert-publication-absence :assert-source-absence) #{:gesso_live_outbox}
        nil))
    (catch clojure.lang.ExceptionInfo e
      (if (::unsupported (ex-data e)) nil (throw e)))))

(defn execute
  "Interpret a complete, supported operation vector on a canonical map
   {table {string-or-uuid-id document}}. Puts replace entire documents in order;
   literal, homogeneous parameter equality, selected row-read, count,
   mapped scalar, publication identity/source absence and selected journal slot/value assertions can abort and return
   the original state. Reads see the tentative state
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
                :assert-mapped (if (mapped-matches! working kind)
                                 (recur working (inc i))
                                 (assoc report :status :predicted :committed? false
                                        :state state :failed-assertion-index i))
                :assert-journal-slot
                (if (journal-slot-matches? working kind)
                  (recur working (inc i))
                  (assoc report :status :predicted :committed? false
                         :state state :failed-assertion-index i))
                :assert-journal-value
                (if (journal-value-matches! working kind)
                  (recur working (inc i))
                  (assoc report :status :predicted :committed? false
                         :state state :failed-assertion-index i))
                :assert-publication-absence
                (if (publication-absent? working (:id kind))
                  (recur working (inc i))
                  (assoc report :status :predicted :committed? false
                         :state state :failed-assertion-index i))
                :assert-source-absence
                (if (source-absent! working (:source kind))
                  (recur working (inc i))
                  (assoc report :status :predicted :committed? false
                         :state state :failed-assertion-index i))
                (:assert-row-count :assert-field-count)
                (if (count-matches? working kind)
                  (recur working (inc i))
                  (assoc report :status :predicted :committed? false
                         :state state :failed-assertion-index i))
                :put (let [[_ table & docs] (nth ops i)]
                       (recur (reduce (fn [s doc] (assoc-in s [table (:xt/id doc)] (normalize-document table doc))) working docs)
                              (inc i))))))))
      (catch clojure.lang.ExceptionInfo e
        (if (::unsupported (ex-data e))
          (assoc report :status :unsupported :reason (:reason (ex-data e)))
          (throw e))))))

(ns gesso.assurance.xtdb-envelope
  "Diagnostic correspondence for the restricted mapped ASSERT/put-docs
   transaction language. Compare caller-supplied expectations with the
   retained decoded source log and a separate provider outcome. Neither
   an authenticated receipt nor proof of execution or writer identity."
  (:require [clojure.string :as str]
            [gesso.assurance.subject :as subject]
            [gesso.assurance.xtdb-source-log :as source-log]
            [xtdb.api :as xt])
  (:import (java.time Instant ZonedDateTime)
           (java.util HexFormat)
           (xtdb.util Iid)))

(def profile :xtdb-2.2.0-beta1/restricted-envelope-v1)
(def ^:private log-profile :xtdb-2.2.0-beta1/decoded-source-log-v1)

(defn- reject! [reason]
  (throw (ex-info "Unsupported expected envelope." {::unsupported true :reason reason})))

(defn- expected-operations! [ops]
  (when-not (and (vector? ops) (<= 1 (count ops) 16384))
    (reject! :unsupported-operation-sequence))
  (let [remaining (volatile! 262144)
        chars (volatile! (* 16 1024 1024))
        charge! (fn [n]
                  (when (neg? (vswap! remaining - n)) (reject! :expected-value-limit)))
        text! (fn [s]
                (when (neg? (vswap! chars - (count s))) (reject! :expected-text-limit))
                s)]
    (letfn [(value! [v depth document?]
              (charge! 1)
              (when (> depth 64) (reject! :expected-depth-limit))
              (cond
                (string? v) (text! v)
                (keyword? v) (do (text! (str v)) v)
                (or (boolean? v) (integer? v) (uuid? v)) v
                (vector? v) (mapv #(value! % (inc depth) document?) v)
                (and document? (map? v))
                (into {} (map (fn [[k child]]
                                (when-not (keyword? k) (reject! :unsupported-document-key))
                                (charge! 1)
                                (text! (str k))
                                [k (value! child (inc depth) true)])) v)
                (and document? (or (instance? Instant v) (instance? ZonedDateTime v))) v
                :else (reject! :unsupported-expected-value)))
            (sql! [[sql & args]]
              (text! sql)
              ;; Only generated assertion text: quoted identifier segments
              ;; may contain $, but no quoted literals, comments, numbered
              ;; parameters, semicolons or ambiguous question marks.
              (let [unquoted (str/replace sql #"\"[a-zA-Z_][a-zA-Z0-9_$]*\"" "identifier")]
                (when-not (and (re-matches #"ASSERT [a-zA-Z0-9_$ \t\r\n(),.*?=<>!+\-\[\]]+" unquoted)
                               (not (re-find #"(?<![a-zA-Z0-9_$])\$" unquoted))
                               (not (str/includes? unquoted "--")))
                  (reject! :unsupported-assertion-text)))
              (let [n (volatile! 0)
                    query (str/replace sql #"\?" (fn [_] (str "$" (vswap! n inc))))]
                (when-not (= @n (count args)) (reject! :unsupported-placeholder-shape))
                {:kind :sql :query query :args (mapv #(value! % 0 false) args)}))
            (put! [op]
              (let [table (second op)]
                (when-not (and (simple-keyword? table)
                               (re-matches #"[a-z][a-z0-9_]*" (name table))
                               (< 2 (count op)))
                  (reject! :unsupported-put-shape))
                (text! (name table))
                (let [docs (mapv (fn [doc]
                                   (when-not (and (map? doc) (contains? doc :xt/id)
                                                  (not-any? #(contains? doc %) [:xt/valid-from :xt/valid-to]))
                                     (reject! :unsupported-document-shape))
                                   (value! doc 0 true)) (subvec op 2))]
                  {:kind :put-docs :table (keyword "public" (name table)) :documents docs
                   :iids (mapv #(.formatHex (HexFormat/of) (Iid/getAsIid (:xt/id %))) docs)
                   :bounds {}})))]
      (mapv (fn [op]
              (charge! 1)
              (when-not (and (vector? op) (seq op)) (reject! :unsupported-operation))
              (cond
                (string? (first op)) (sql! op)
                (= :put-docs (first op)) (put! op)
                :else (reject! :unsupported-operation))) ops))))

(def binding-profile :xtdb-2.2.0-beta1/expected-transition-envelope-v1)
(def binding-keys [:envelope-profile :envelope-fingerprint])

(defn binding-status
  "Shape only. Older witnesses have no envelope binding; a present digest
   is not yet evidence of actual submission or execution."
  [witness]
  (cond
    (not-any? #(contains? witness %) binding-keys) :not-recorded
    (and (= binding-profile (:envelope-profile witness))
         (string? (:envelope-fingerprint witness))
         (re-matches #"sha256:[0-9a-f]{64}" (:envelope-fingerprint witness))) :recorded-unchecked
    :else :invalid))

(defn- digest-value [v]
  ;; Tag containers as well as non-EDN leaves: a literal vector cannot
  ;; alias the encoding of a UUID or temporal value.
  (cond
    (map? v) [:map (into {} (map (fn [[k child]] [k (digest-value child)])) v)]
    (vector? v) [:vector (mapv digest-value v)]
    (uuid? v) [:uuid (str v)]
    (instance? Instant v) [:instant (str v)]
    (instance? ZonedDateTime v) [:zoned-date-time (str v)]
    :else v))

(defn- transition-witness! [ops]
  (let [op (when (vector? ops) (peek ops))]
    (when-not (and (vector? op) (= 3 (count op))
                   (= [:put-docs :gesso_transition_evidence] (subvec op 0 2))
                   (map? (peek op)) (= 4 (:profile-version (peek op))))
      (reject! :invalid-transition-envelope-witness))
    (peek op)))

(defn- binding-evidence! [ops]
  {:envelope-profile binding-profile
   :envelope-fingerprint
   (:fingerprint (subject/describe-subject
                  {:kind :rule :id ::transition-envelope
                   :interface {:profile binding-profile
                               :operations (digest-value (expected-operations! ops))}}))})

(defn bind-transition-envelope!
  "Bind the expected ordered restricted envelope to its final v4 witness
   before submission. The digest excludes ONLY its own two binding fields
   in that final witness. Subject-encoder limits also apply. No transaction
   metadata, source-log acquisition or writer authentication is asserted."
  [ops]
  (let [witness (transition-witness! ops)]
    (when-not (= :not-recorded (binding-status witness))
      (reject! :already-bound-transition-envelope))
    (assoc-in ops [(dec (count ops)) 2] (merge witness (binding-evidence! ops)))))

(defn bound-transition-envelope?
  "Recompute the expected-envelope binding; not a source-log check."
  [ops]
  (let [witness (transition-witness! ops)]
    (and (= :recorded-unchecked (binding-status witness))
         (= (select-keys witness binding-keys)
            (binding-evidence! (assoc-in ops [(dec (count ops)) 2]
                                         (apply dissoc witness binding-keys)))))))

(defn check-envelope!
  "Compare a nonempty vector of generated ASSERT and canonical public
   put-docs operations (without temporal options) against one exact
   transaction and expected boolean commit outcome. Returns :matched,
   :mismatch, :unsupported or :unavailable; no raw values or SQL in the
   report. Match is decoded operation equality only; does not cover
   transaction metadata, caller bytes, writer identity or certification."
  [node tx-id ops committed?]
  (let [report (fn [status reason]
                 (cond-> {:status status :profile profile :authority :none :certificate :not-issued}
                   reason (assoc :reason reason)))]
    (try
      (if-not (boolean? committed?)
        (report :unsupported :invalid-expected-outcome)
        (let [expected (expected-operations! ops)
              observed (source-log/observe-source-message! node tx-id)]
          (case (:status observed)
            :unavailable (report :unavailable (:reason observed))
            :unsupported (report :unsupported (:reason observed))
            :source-message-observed
            (cond
              (not= log-profile (:provider-profile observed)) (report :unsupported :source-profile-mismatch)
              (not= tx-id (:tx-id observed)) (report :mismatch :source-transaction-identity-mismatch)
              (not= expected (:operations observed)) (report :mismatch :operation-sequence-mismatch)
              :else
              (let [outcomes (xt/q node ["SELECT committed FROM xt.txs WHERE _id = ?" tx-id])]
                (cond
                  (empty? outcomes) (report :unavailable :transaction-outcome-unavailable)
                  (not (and (= 1 (count outcomes)) (boolean? (:committed (first outcomes)))))
                  (report :unsupported :invalid-transaction-outcome)
                  (not= [{:committed committed?}] outcomes) (report :mismatch :transaction-outcome-mismatch)
                  :else (assoc (report :matched nil) :tx-id tx-id :committed? committed?
                               :operation-count (count expected)))))
            (report :unsupported :unexpected-source-observation))))
      (catch InterruptedException e
        (.interrupt (Thread/currentThread))
        (throw e))
      (catch clojure.lang.ExceptionInfo e
        (if (::unsupported (ex-data e))
          (report :unsupported (:reason (ex-data e)))
          (report :unavailable :envelope-check-failed)))
      (catch Exception _ (report :unavailable :envelope-check-failed)))))

(defn- observed-restricted-operations! [operations]
  ;; Require an exact round trip through the restricted caller language.
  ;; Never discard bounds, IID, placeholder order or extra fields.
  (when-not (and (vector? operations) (<= 1 (count operations) 16384))
    (reject! :unsupported-operation-sequence))
  (let [ops (mapv (fn [{:keys [kind query args table documents]}]
                    (case kind
                      :sql (do
                             (when-not (and (string? query) (vector? args))
                               (reject! :unsupported-observed-sql))
                             (into [(str/replace query #"(?<![a-zA-Z0-9_$])\$[0-9]+" "?")] args))
                      :put-docs (do
                                  (when-not (and (keyword? table) (= "public" (namespace table))
                                                 (vector? documents))
                                    (reject! :unsupported-observed-put))
                                  (into [:put-docs (keyword (name table))] documents))
                      (reject! :unsupported-observed-operation))) operations)]
    (when-not (= operations (expected-operations! ops))
      (reject! :unsupported-observed-envelope))
    ops))

(defn check-recorded-transition!
  "Check a committed transition's decoded source-log envelope against its
   independently acquired, pinned durable v4 witness. No caller operation
   vector is required. The final logged witness must match exactly, its
   binding must recompute, and the ledger must independently show commit.
   This checks decoded correspondence, not writer authority or execution."
  [node tx-id witness]
  (let [report (fn [status reason]
                 (cond-> {:status status :profile binding-profile :authority :none :certificate :not-issued}
                   reason (assoc :reason reason)))]
    (try
      (cond
        (not (and (map? witness) (= 4 (:profile-version witness)) (some? (:xt/id witness))))
        (report :unsupported :invalid-transition-envelope-witness)
        (not= :recorded-unchecked (binding-status witness))
        (report :unsupported :missing-or-invalid-envelope-binding)
        :else
        (let [observed (source-log/observe-source-message! node tx-id)]
          (case (:status observed)
            :unavailable (report :unavailable (:reason observed))
            :unsupported (report :unsupported (:reason observed))
            :source-message-observed
            (cond
              (not= log-profile (:provider-profile observed)) (report :unsupported :source-profile-mismatch)
              (not= tx-id (:tx-id observed)) (report :mismatch :source-transaction-identity-mismatch)
              :else
              (let [ops (observed-restricted-operations! (:operations observed))]
                (cond
                  (not= witness (transition-witness! ops)) (report :mismatch :source-witness-mismatch)
                  (not (bound-transition-envelope? ops)) (report :mismatch :envelope-fingerprint-mismatch)
                  :else
                  (let [outcomes (xt/q node ["SELECT committed FROM xt.txs WHERE _id = ?" tx-id])]
                    (cond
                      (empty? outcomes) (report :unavailable :transaction-outcome-unavailable)
                      (not (and (= 1 (count outcomes)) (boolean? (:committed (first outcomes)))))
                      (report :unsupported :invalid-transaction-outcome)
                      (not= [{:committed true}] outcomes) (report :mismatch :transaction-outcome-mismatch)
                      :else (assoc (report :matched nil) :tx-id tx-id :operation-count (count ops)
                                   :envelope-fingerprint (:envelope-fingerprint witness)))))))
            (report :unsupported :unexpected-source-observation))))
      (catch InterruptedException e
        (.interrupt (Thread/currentThread))
        (throw e))
      (catch clojure.lang.ExceptionInfo e
        (if (::unsupported (ex-data e))
          (report :unsupported (:reason (ex-data e)))
          (report :unavailable :envelope-check-failed)))
      (catch Exception _ (report :unavailable :envelope-check-failed)))))

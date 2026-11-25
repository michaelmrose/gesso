(ns gesso.assurance.reference-arrow-schema
  "Internal qualification of selected Arrow argument field schemas."
  (:require [gesso.assurance.reference-arrow-framing :refer [require! field!]])
  (:import (org.apache.arrow.flatbuf Field Type Union UnionMode Int FixedSizeBinary Timestamp TimeUnit)))

(declare scalar-kind!)

(def ^:private union-leg-kinds
  {"i64" :integer "utf8" :string "bool" :boolean "keyword" :keyword "uuid" :uuid})

(defn- selected-union-kind! [^Field f field-name leg-kinds]
  (let [n (.childrenLength f)
        ^Union u (.type f (Union.))]
    (field! f field-name Type/Union false n)
    (require! (and (<= 2 n (count leg-kinds)) u
                   (= UnionMode/Dense (.mode u)) (zero? (.typeIdsLength u)))
              :unsupported-arrow-list-union)
    (let [kinds (mapv (fn [i]
                        (let [^Field leg (.children f i)
                              name (.name leg)
                              kind (get leg-kinds name)]
                          (require! kind :unsupported-arrow-list-union-leg)
                          (let [actual (scalar-kind! leg name (if (= :timestamp-utc kind)
                                                               #{:timestamp :timestamp-utc} #{kind}))]
                            (require! (= kind actual) :unsupported-arrow-list-union-leg)
                            actual)))
                      (range n))]
      (require! (= n (count (distinct kinds))) :unsupported-arrow-list-union-leg)
      [:union kinds])))

(defn- union-kind! [f temporal?]
  (selected-union-kind! f "$data$"
                        (cond-> union-leg-kinds
                          temporal? (assoc "timestamp-tz-micro-utc" :timestamp-utc))))

(defn- document-id-union-kind! [f]
  (selected-union-kind! f "_id" (dissoc union-leg-kinds "bool")))

(defn- struct-key! [name allowed depth]
  (require! (and name (<= 1 (count name) 4095)) :unsupported-arrow-struct-key)
  (if (contains? allowed :document-keys)
    (cond
      (= "_id" name)
      (do (require! (zero? depth) :unsupported-arrow-struct-key) "xt/id")
      (and (contains? allowed :document-valid-time)
           (#{"_valid_from" "_valid_to"} name))
      (do (require! (zero? depth) :unsupported-arrow-struct-key)
          (if (= "_valid_from" name) "xt/valid-from" "xt/valid-to"))
      :else
      (let [split (.lastIndexOf ^String name "$")
            ns-part (when (not= -1 split) (subs name 0 split))
            local (subs name (inc split))]
        (require! (and (re-matches #"[a-z][a-z0-9]*(?:_[a-z0-9]+)*" local)
                       (or (nil? ns-part)
                           (and (not= "xt" ns-part)
                                (re-matches #"[a-z][a-z0-9]*(?:\$[a-z][a-z0-9]*)*" ns-part))))
                  :unsupported-arrow-struct-key)
        (str (when ns-part (str (.replace ^String ns-part \$ \.) "/"))
             (.replace ^String local \_ \-))))
    (do (require! (re-matches #"[a-z][a-z0-9]*" name) :unsupported-arrow-struct-key)
        name)))

(defn- scalar-kind-at! [^Field f field-name allowed depth]
  (let [nullable? (and (contains? allowed :nullable-field) (.nullable f))
        allowed (disj allowed :nullable-field)
        type (.typeType f)
        extension? (pos? (.customMetadataLength f))
        kind (if extension?
               (let [_ (require! (= 2 (.customMetadataLength f)) :unsupported-arrow-field)
                     metadata (into {} (map (fn [j]
                                             (let [kv (.customMetadata f j)]
                                               [(.key kv) (.value kv)]))
                                           (range (.customMetadataLength f))))
                     name (get metadata "ARROW:extension:name")]
                 (require! (and (= 2 (.customMetadataLength f))
                                (= {"ARROW:extension:name" name
                                    "ARROW:extension:metadata" ""} metadata))
                           :unsupported-arrow-field)
                 (cond
                   (and (= Type/Utf8 type) (= "xt/clj-keyword" name)) :keyword
                   (and (= Type/FixedSizeBinary type) (= "uuid" name)) :uuid
                   :else (require! false :unsupported-arrow-field)))
               (get {Type/Int :integer Type/Bool :boolean Type/Utf8 :string
                     Type/Timestamp :timestamp Type/Struct_ :struct-map Type/List (cond
                                                           (contains? allowed :empty-list) :empty-list
                                                           (contains? allowed :temporal-union-list) :temporal-union-list
                                                           (contains? allowed :union-list) :union-list
                                                           (contains? allowed :temporal-list) :temporal-list
                                                           (contains? allowed :scalar-list) :scalar-list
                                                           :else :int64-list)} type))]
    (require! (contains? allowed kind) :unsupported-arrow-field)
    (require! (or (not nullable?) (#{:integer :boolean :string :keyword :uuid :timestamp} kind)
                  (and (= :empty-list kind) (contains? allowed :nullable-list-fields))
                  (and (= :struct-map kind) (contains? allowed :nullable-map-fields)))
              :unsupported-arrow-nullable-field)
    (if (#{:keyword :uuid} kind)
      (require! (and (= field-name (.name f)) (= (boolean nullable?) (.nullable f))
                     (zero? (.childrenLength f)) (nil? (.dictionary f)))
                :unsupported-arrow-field)
      (field! f field-name type (boolean nullable?) (cond
                                      (= :struct-map kind) (.childrenLength f)
                                      (#{:int64-list :scalar-list :temporal-list :union-list :temporal-union-list :empty-list} kind) 1
                                      :else 0)))
    (when (= :integer kind)
      (let [^Int t (.type f (Int.))]
        (require! (and t (= 64 (.bitWidth t)) (.isSigned t)) :unsupported-integer-type)))
    (when (= :uuid kind)
      (let [^FixedSizeBinary t (.type f (FixedSizeBinary.))]
        (require! (and t (= 16 (.byteWidth t))) :unsupported-uuid-width)))
    (cond
      (= :struct-map kind)
      (let [n (.childrenLength f)]
        (require! (< depth 8) :arrow-struct-depth-limit)
        (require! (<= (if (contains? allowed :empty-struct) 0 1) n 256)
                  :unsupported-arrow-struct-field-count)
        (let [fields (mapv (fn [i]
                             (let [^Field child (.children f i)
                                   name (.name child)]
                               (let [key (struct-key! name allowed depth)
                                     valid-time? (#{"xt/valid-from" "xt/valid-to"} key)
                                     nullable-field? (and (contains? allowed :nullable-struct-fields)
                                                          (not= "xt/id" key) (.nullable child))
                                     null-only? (and nullable-field?
                                                     (contains? allowed :null-only-fields)
                                                     (= Type/Null (.typeType child)))
                                     child-kind (cond
                                                  valid-time?
                                                  (let [k (scalar-kind-at! child name
                                                                           (cond-> #{:timestamp :timestamp-utc}
                                                                             (and nullable-field?
                                                                                  (contains? allowed :nullable-document-valid-time))
                                                                             (conj :nullable-field))
                                                                           (inc depth))]
                                                    (require! (= :timestamp-utc k) :unsupported-arrow-put-bound-type)
                                                    k)
                                                  (and (= "xt/id" key)
                                                       (contains? allowed :mixed-document-ids)
                                                       (= Type/Union (.typeType child)))
                                                  (document-id-union-kind! child)
                                                  null-only?
                                                  (do (field! child name Type/Null true 0)
                                                      :null-only-field)
                                                  :else (scalar-kind-at! child name
                                                                   (cond-> allowed nullable-field?
                                                                     (conj :nullable-field))
                                                                   (inc depth)))]
                                 [key (if (and nullable-field? (not null-only?))
                                        [:nullable child-kind] child-kind)])))
                           (range n))]
          (require! (= n (count (distinct (map first fields)))) :duplicate-arrow-struct-key)
          [:struct fields]))
      (#{:int64-list :scalar-list :temporal-list :union-list :temporal-union-list :empty-list} kind)
      (let [^Field child (.children f 0)]
        (require! (< depth 8) :arrow-list-depth-limit)
        [:list (cond
                 (and (contains? allowed :recursive-list) (= Type/List (.typeType child)))
                 (scalar-kind-at! child "$data$" allowed (inc depth))
                 (and (= :empty-list kind) (= Type/Null (.typeType child)))
                 (do (field! child "$data$" Type/Null false 0) :empty-child)
                 (and (#{:union-list :temporal-union-list :empty-list} kind) (= Type/Union (.typeType child)))
                 (union-kind! child (not= :union-list kind))
                 :else (scalar-kind! child "$data$"
                               (case kind
                                 :int64-list #{:integer}
                                 :scalar-list #{:integer :boolean :string :keyword :uuid}
                                 (:temporal-list :union-list :temporal-union-list :empty-list)
                                 #{:integer :boolean :string :keyword :uuid :timestamp :timestamp-utc})))])
      (= :timestamp kind)
      (let [^Timestamp t (.type f (Timestamp.))
            zone (when t (.timezone t))]
        (require! (and t (= TimeUnit/MICROSECOND (.unit t))
                       (or (= "Z" zone)
                           (and (= "UTC" zone) (contains? allowed :timestamp-utc))))
                  :unsupported-arrow-timestamp-type)
        (if (= "UTC" zone) :timestamp-utc :timestamp))
      :else kind)))

(defn scalar-kind! [f field-name allowed]
  (scalar-kind-at! f field-name allowed 0))


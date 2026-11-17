(ns gesso.choreo.internal.nominal-identity
  "Internal nominal Choreo identity categories. These are not application
   data, public wire values, or replacements for the tagged identity API.

   Their purpose is to prevent accidental substitution of unrelated identity
   categories within checked Gesso execution paths. The public map-to-nominal
   adapter remains in the experimental test bridge pending migration.

   Identity constructors establish a category, not authentication, authority,
   or admitted operation provenance. Direct deftype constructors are not a
   security boundary against a hostile same-process application author.")

;; Values are opaque to application code. Runtime equality is category-aware,
;; matching the existing tagged-map identities without conflating raw IDs.
;; Extension fields allow lossless conversion of *open* legacy tagged maps;
;; equality and hashing include extensions because ordinary map equality does.
;; Use ordinary deftype, not defrecord, to avoid map-associative mutation.
(deftype Principal [raw extensions]
  #?(:clj Object :cljs IEquiv)
  #?(:clj (equals [_ other]
            (and (instance? Principal other)
                 (= raw (.-raw ^Principal other))
                 (= extensions (.-extensions ^Principal other))))
     :cljs (-equiv [_ other]
             (and (instance? Principal other)
                  (= raw (.-raw other))
                  (= extensions (.-extensions other)))))
  #?(:clj (hashCode [_] (hash [:principal raw extensions]))
     :cljs IHash)
  #?(:cljs (-hash [_] (hash [:principal raw extensions])))
  #?(:clj clojure.lang.IHashEq)
  #?(:clj (hasheq [_] (hash [:principal raw extensions]))))

(deftype Actor [raw extensions]
  #?(:clj Object :cljs IEquiv)
  #?(:clj (equals [_ other]
            (and (instance? Actor other)
                 (= raw (.-raw ^Actor other))
                 (= extensions (.-extensions ^Actor other))))
     :cljs (-equiv [_ other]
             (and (instance? Actor other)
                  (= raw (.-raw other))
                  (= extensions (.-extensions other)))))
  #?(:clj (hashCode [_] (hash [:actor raw extensions]))
     :cljs IHash)
  #?(:cljs (-hash [_] (hash [:actor raw extensions])))
  #?(:clj clojure.lang.IHashEq)
  #?(:clj (hasheq [_] (hash [:actor raw extensions]))))

(deftype Authority [raw extensions]
  #?(:clj Object :cljs IEquiv)
  #?(:clj (equals [_ other]
            (and (instance? Authority other)
                 (= raw (.-raw ^Authority other))
                 (= extensions (.-extensions ^Authority other))))
     :cljs (-equiv [_ other]
             (and (instance? Authority other)
                  (= raw (.-raw other))
                  (= extensions (.-extensions other)))))
  #?(:clj (hashCode [_] (hash [:authority raw extensions]))
     :cljs IHash)
  #?(:cljs (-hash [_] (hash [:authority raw extensions])))
  #?(:clj clojure.lang.IHashEq)
  #?(:clj (hasheq [_] (hash [:authority raw extensions]))))
(deftype Host [raw extensions]
  #?(:clj Object :cljs IEquiv)
  #?(:clj (equals [_ other]
            (and (instance? Host other)
                 (= raw (.-raw ^Host other))
                 (= extensions (.-extensions ^Host other))))
     :cljs (-equiv [_ other]
             (and (instance? Host other)
                  (= raw (.-raw other))
                  (= extensions (.-extensions other)))))
  #?(:clj (hashCode [_] (hash [:host raw extensions]))
     :cljs IHash)
  #?(:cljs (-hash [_] (hash [:host raw extensions])))
  #?(:clj clojure.lang.IHashEq)
  #?(:clj (hasheq [_] (hash [:host raw extensions]))))
(deftype CommandId [raw extensions]
  #?(:clj Object :cljs IEquiv)
  #?(:clj (equals [_ other]
            (and (instance? CommandId other)
                 (= raw (.-raw ^CommandId other))
                 (= extensions (.-extensions ^CommandId other))))
     :cljs (-equiv [_ other]
             (and (instance? CommandId other)
                  (= raw (.-raw other))
                  (= extensions (.-extensions other)))))
  #?(:clj (hashCode [_] (hash [:command raw extensions]))
     :cljs IHash)
  #?(:cljs (-hash [_] (hash [:command raw extensions])))
  #?(:clj clojure.lang.IHashEq)
  #?(:clj (hasheq [_] (hash [:command raw extensions]))))
(deftype ExecutionId [raw extensions]
  #?(:clj Object :cljs IEquiv)
  #?(:clj (equals [_ other]
            (and (instance? ExecutionId other)
                 (= raw (.-raw ^ExecutionId other))
                 (= extensions (.-extensions ^ExecutionId other))))
     :cljs (-equiv [_ other]
             (and (instance? ExecutionId other)
                  (= raw (.-raw other))
                  (= extensions (.-extensions other)))))
  #?(:clj (hashCode [_] (hash [:execution raw extensions]))
     :cljs IHash)
  #?(:cljs (-hash [_] (hash [:execution raw extensions])))
  #?(:clj clojure.lang.IHashEq)
  #?(:clj (hasheq [_] (hash [:execution raw extensions]))))

(defn principal [raw]
  {:pre [(some? raw)]}
  (Principal. raw {}))

(defn actor [raw]
  {:pre [(some? raw)]}
  (Actor. raw {}))

(defn authority [raw]
  {:pre [(some? raw)]}
  (Authority. raw {}))

(defn host [raw]
  {:pre [(some? raw)]}
  (Host. raw {}))

(defn command-id [raw]
  {:pre [(some? raw)]}
  (CommandId. raw {}))

(defn execution-id [raw]
  {:pre [(some? raw)]}
  (ExecutionId. raw {}))


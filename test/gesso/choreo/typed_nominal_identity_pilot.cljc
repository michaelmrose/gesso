(ns gesso.choreo.typed-nominal-identity-pilot
  "Opt-in proof of concept: ordinary defn, six distinct internal nominal
   identities, and seven independently checked runtime handoffs.

   Deliberately separate from production gesso.choreo.identity: this pilot
   must not change the public tagged-map format, wire format, or app contract.
   Preconditions target accidental misuse, not adversarial callers."
  (:require [clojure.test :refer [deftest is testing]]))

;; Values are opaque to application code. Runtime equality is category-aware,
;; matching the existing tagged-map identities without conflating raw IDs.
;; Use ordinary deftype, not defrecord, to avoid map-associative mutation.
(deftype Principal [raw]
  #?(:clj Object :cljs IEquiv)
  #?(:clj (equals [_ other]
            (and (instance? Principal other)
                 (= raw (.-raw ^Principal other))))
     :cljs (-equiv [_ other]
             (and (instance? Principal other)
                  (= raw (.-raw other)))))
  #?(:clj (hashCode [_] (hash [:principal raw]))
     :cljs IHash)
  #?(:cljs (-hash [_] (hash [:principal raw])))
  #?(:clj clojure.lang.IHashEq)
  #?(:clj (hasheq [_] (hash [:principal raw]))))

(deftype Actor [raw]
  #?(:clj Object :cljs IEquiv)
  #?(:clj (equals [_ other]
            (and (instance? Actor other)
                 (= raw (.-raw ^Actor other))))
     :cljs (-equiv [_ other]
             (and (instance? Actor other)
                  (= raw (.-raw other)))))
  #?(:clj (hashCode [_] (hash [:actor raw]))
     :cljs IHash)
  #?(:cljs (-hash [_] (hash [:actor raw])))
  #?(:clj clojure.lang.IHashEq)
  #?(:clj (hasheq [_] (hash [:actor raw]))))

(deftype Authority [raw]
  #?(:clj Object :cljs IEquiv)
  #?(:clj (equals [_ other]
            (and (instance? Authority other)
                 (= raw (.-raw ^Authority other))))
     :cljs (-equiv [_ other]
             (and (instance? Authority other)
                  (= raw (.-raw other)))))
  #?(:clj (hashCode [_] (hash [:authority raw]))
     :cljs IHash)
  #?(:cljs (-hash [_] (hash [:authority raw])))
  #?(:clj clojure.lang.IHashEq)
  #?(:clj (hasheq [_] (hash [:authority raw]))))
(deftype Host [raw]
  #?(:clj Object :cljs IEquiv)
  #?(:clj (equals [_ other]
            (and (instance? Host other)
                 (= raw (.-raw ^Host other))))
     :cljs (-equiv [_ other]
             (and (instance? Host other)
                  (= raw (.-raw other)))))
  #?(:clj (hashCode [_] (hash [:host raw]))
     :cljs IHash)
  #?(:cljs (-hash [_] (hash [:host raw])))
  #?(:clj clojure.lang.IHashEq)
  #?(:clj (hasheq [_] (hash [:host raw]))))
(deftype CommandId [raw]
  #?(:clj Object :cljs IEquiv)
  #?(:clj (equals [_ other]
            (and (instance? CommandId other)
                 (= raw (.-raw ^CommandId other))))
     :cljs (-equiv [_ other]
             (and (instance? CommandId other)
                  (= raw (.-raw other)))))
  #?(:clj (hashCode [_] (hash [:command raw]))
     :cljs IHash)
  #?(:cljs (-hash [_] (hash [:command raw])))
  #?(:clj clojure.lang.IHashEq)
  #?(:clj (hasheq [_] (hash [:command raw]))))
(deftype ExecutionId [raw]
  #?(:clj Object :cljs IEquiv)
  #?(:clj (equals [_ other]
            (and (instance? ExecutionId other)
                 (= raw (.-raw ^ExecutionId other))))
     :cljs (-equiv [_ other]
             (and (instance? ExecutionId other)
                  (= raw (.-raw other)))))
  #?(:clj (hashCode [_] (hash [:execution raw]))
     :cljs IHash)
  #?(:cljs (-hash [_] (hash [:execution raw])))
  #?(:clj clojure.lang.IHashEq)
  #?(:clj (hasheq [_] (hash [:execution raw]))))

(defn principal [raw]
  {:pre [(some? raw)]}
  (Principal. raw))

(defn actor [raw]
  {:pre [(some? raw)]}
  (Actor. raw))

(defn authority [raw]
  {:pre [(some? raw)]}
  (Authority. raw))

(defn host [raw]
  {:pre [(some? raw)]}
  (Host. raw))

(defn command-id [raw]
  {:pre [(some? raw)]}
  (CommandId. raw))

(defn execution-id [raw]
  {:pre [(some? raw)]}
  (ExecutionId. raw))

;; A seven-stage example uses seven actual ordinary defns. All seven must
;; reject a wrong category when called *directly*. The type hint is useful
;; for interop but does NOT perform the runtime check; the :pre does.
(defn step-7 [^Principal p]
  {:pre [(instance? Principal p)]}
  p)
(defn step-6 [^Principal p]
  {:pre [(instance? Principal p)]}
  (step-7 p))
(defn step-5 [^Principal p]
  {:pre [(instance? Principal p)]}
  (step-6 p))
(defn step-4 [^Principal p]
  {:pre [(instance? Principal p)]}
  (step-5 p))
(defn step-3 [^Principal p]
  {:pre [(instance? Principal p)]}
  (step-4 p))
(defn step-2 [^Principal p]
  {:pre [(instance? Principal p)]}
  (step-3 p))
(defn step-1 [^Principal p]
  {:pre [(instance? Principal p)]}
  (step-2 p))

(def stages [step-1 step-2 step-3 step-4 step-5 step-6 step-7])

(deftest nominal-categories-and-seven-direct-entry-points
  (let [good (principal :same-raw-id)
        wrong [(actor :same-raw-id)
               (authority :same-raw-id)
               (host :same-raw-id)
               (command-id :same-raw-id)
               (execution-id :same-raw-id)
               {:raw :same-raw-id}
               nil]]
    (is (= 7 (count stages)))
    (doseq [[idx stage] (map-indexed vector stages)]
      (testing (str "stage " (inc idx) " independently enforces Principal")
        (is (identical? good (stage good)))
        (doseq [bad wrong]
          (is (thrown? #?(:clj AssertionError :cljs js/Error)
                       (stage bad))))))))

(deftest nominal-equality-and-hash
  (let [constructors [principal actor authority host command-id execution-id]
        values (mapv #(% :same) constructors)]
    (doseq [[i make] (map-indexed vector constructors)]
      (let [a (make :same)
            b (make :same)]
        (testing (str "category " i " has value equality and hashing")
          (is (= a b))
          (is (= (hash a) (hash b)))
          (is (= :found (get {a :found} b)))
          (is (= 1 (count (hash-set a b))))))
      (doseq [[j other] (map-indexed vector values)
              :when (not= i j)]
        (is (not= (make :same) other))))))

#?(:clj
   (defn -main [& _]
     (let [result (clojure.test/run-tests
                   'gesso.choreo.typed-nominal-identity-pilot)]
       (when (pos? (+ (:fail result) (:error result)))
         (throw (ex-info "Nominal pilot failed." result)))
       :ok)))

#?(:cljs
   (defn -main [& _]
     (cljs.test/run-tests 'gesso.choreo.typed-nominal-identity-pilot)))

#?(:cljs (set! *main-cli-fn* -main))

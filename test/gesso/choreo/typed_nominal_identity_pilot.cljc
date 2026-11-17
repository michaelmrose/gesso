(ns gesso.choreo.typed-nominal-identity-pilot
  "Opt-in proof of concept: ordinary defn, six distinct *production* nominal
   identities, and seven independently checked runtime handoffs.

   Deliberately separate from public gesso.choreo.identity: this pilot
   must not change the public tagged-map format, wire format, or app contract.
   Preconditions target accidental misuse, not adversarial callers."
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.choreo.internal.nominal-identity :as internal]))

;; The six runtime types and validated constructors are now production
;; internals. These ordinary test-only facades retain the existing pilot call
;; API; they do not define alternative classes or replace defn.
(defn principal [raw]
  {:pre [(some? raw)]}
  (internal/principal raw))

(defn actor [raw]
  {:pre [(some? raw)]}
  (internal/actor raw))

(defn authority [raw]
  {:pre [(some? raw)]}
  (internal/authority raw))

(defn host [raw]
  {:pre [(some? raw)]}
  (internal/host raw))

(defn command-id [raw]
  {:pre [(some? raw)]}
  (internal/command-id raw))

(defn execution-id [raw]
  {:pre [(some? raw)]}
  (internal/execution-id raw))

;; A seven-stage example uses seven actual ordinary defns. All seven must
;; reject a wrong category when called *directly*. The type hint is useful
;; for interop but does NOT perform the runtime check; the :pre does.
(defn step-7 [^gesso.choreo.internal.nominal_identity.Principal p]
  {:pre [(instance? gesso.choreo.internal.nominal_identity.Principal p)]}
  p)
(defn step-6 [^gesso.choreo.internal.nominal_identity.Principal p]
  {:pre [(instance? gesso.choreo.internal.nominal_identity.Principal p)]}
  (step-7 p))
(defn step-5 [^gesso.choreo.internal.nominal_identity.Principal p]
  {:pre [(instance? gesso.choreo.internal.nominal_identity.Principal p)]}
  (step-6 p))
(defn step-4 [^gesso.choreo.internal.nominal_identity.Principal p]
  {:pre [(instance? gesso.choreo.internal.nominal_identity.Principal p)]}
  (step-5 p))
(defn step-3 [^gesso.choreo.internal.nominal_identity.Principal p]
  {:pre [(instance? gesso.choreo.internal.nominal_identity.Principal p)]}
  (step-4 p))
(defn step-2 [^gesso.choreo.internal.nominal_identity.Principal p]
  {:pre [(instance? gesso.choreo.internal.nominal_identity.Principal p)]}
  (step-3 p))
(defn step-1 [^gesso.choreo.internal.nominal_identity.Principal p]
  {:pre [(instance? gesso.choreo.internal.nominal_identity.Principal p)]}
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

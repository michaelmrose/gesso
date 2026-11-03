(ns gesso.assurance.subject-test
  "v1212: adversarial regression coverage for the *unadmitted*, structural
   subject identities introduced in v1211.

   These tests establish encoding stability and rejection of normalization
   collisions, not collision-free SHA-256 or denotational equivalence. They do
   not certify any subject, proof, production implementation, or assurance
   dependency; later R4 layers must establish those guarantees."
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing]]
            [gesso.assurance.subject :as subject]))

(def ^:private base
  {:kind :operation
   :id :example/claim
   :interface
   {:reads #{:request/status :request/score}
    :guards [:and
             [:> [:field :request/score] 7]
             [:in [:field :request/status] #{:open :queued}]]
    :effects [[:set :request/status :accepted]]
    :opaque {:eligibility :external/trusted-predicate}
    :options {:max-retries 2 :strict? true :default nil}}})

(defn- fingerprint [x]
  (:fingerprint (subject/describe-subject x)))

(defn- invalid [x]
  (try
    (subject/describe-subject x)
    nil
    (catch clojure.lang.ExceptionInfo e
      (ex-data e))))

(defn- invalid-reason [x]
  (:reason (invalid x)))

(defn- changed [path value]
  (assoc-in base path value))

(deftest identity-is-explicitly-descriptive-and-unadmitted
  (let [description (subject/describe-subject base)]
    (is (= #{:type :version :subject :fingerprint :checking :admission}
           (set (keys description))))
    (is (= subject/identity-type (:type description)))
    (is (= :gesso.assurance.subject/unadmitted-identity (:type description)))
    (is (= subject/identity-version (:version description)))
    (is (= 1 (:version description)))
    (is (= {:kind :operation :id :example/claim} (:subject description)))
    (is (re-matches #"sha256:[0-9a-f]{64}" (:fingerprint description)))
    (is (= :not-performed (:checking description)))
    (is (= :not-admitted (:admission description)))
    (is (not (contains? description :certificate)))
    (is (not (contains? description :valid?)))
    (is (not (contains? description :proof)))))

(deftest input-representation-does-not-change-identity
  (let [a (edn/read-string
           "{:kind :operation, :id :example/claim, :interface {:a 1, :b #{:x :y}, :c [7 8], :d {:z 0 :w false}}}")
        b (edn/read-string
           "{\n :interface {:d {:w false, :z 0}, :c [7 8], :b #{:y :x}, :a 1N}\n :id :example/claim, :kind :operation}")]
    (is (= (fingerprint a) (fingerprint b)))
    (is (true? (subject/same-interface? a b)))
    (is (= (fingerprint a) (fingerprint (into (sorted-map) a))))
    (is (= (fingerprint a) (fingerprint (into (array-map) (reverse (seq a))))))
    (is (= (fingerprint a) (fingerprint (edn/read-string (pr-str a))))))
  (let [identity (fingerprint base)]
    (dotimes [_ 10]
      (is (= identity (fingerprint base))))))

(deftest metadata-does-not-pollute-identity
  (let [decorated (-> base
                      (with-meta {:file "changed.clj" :line 999})
                      (assoc-in [:interface :guards]
                                (with-meta (get-in base [:interface :guards])
                                  {:line 12 :origin :generated})))
        nested (assoc-in base [:interface :options]
                         (with-meta (get-in base [:interface :options])
                           {:source-file "different.clj"}))]
    (is (= (fingerprint base) (fingerprint decorated)))
    (is (= (fingerprint base) (fingerprint nested)))
    (is (true? (subject/same-interface? base decorated)))))

(deftest all-subject-fields-belong-to-structural-identity
  (let [original (fingerprint base)
        mutations [(assoc base :kind :protocol)
                   (assoc base :id :example/release)
                   (changed [:interface :reads] #{:request/status})
                   (changed [:interface :guards 1 2] 8)
                   (changed [:interface :effects 0 2] :queued)
                   (changed [:interface :opaque :eligibility] :external/new-service)
                   (changed [:interface :options :max-retries] 3)
                   (changed [:interface :options :strict?] false)
                   (changed [:interface :options :default] :not-specified)
                   (assoc-in base [:interface :extra] nil)
                   (update base :interface dissoc :opaque)
                   (update base :interface dissoc :options)]]
    (doseq [mutant mutations]
      (testing (pr-str mutant)
        (is (not= original (fingerprint mutant)))
        (is (false? (subject/same-interface? base mutant)))))))

(deftest nested-map-order-and-set-order-are-immaterial
  (let [reordered (-> base
                      (assoc-in [:interface :reads]
                                (into (sorted-set) [:request/score :request/status]))
                      (assoc-in [:interface :guards 2 2]
                                (into (sorted-set) [:queued :open]))
                      (update :interface
                              (fn [m] (into (array-map) (reverse (seq m))))))]
    (is (= (fingerprint base) (fingerprint reordered)))
    (is (true? (subject/same-interface? base reordered)))))

(deftest ordered-vectors-cannot-be-silently-reordered
  (let [effects (get-in base [:interface :effects])
        with-extra (changed [:interface :effects]
                            (conj effects [:set :request/score 8]))
        reversed (changed [:interface :effects]
                          (vec (reverse (get-in with-extra [:interface :effects]))))]
    (is (not= (fingerprint with-extra) (fingerprint reversed)))
    (is (not= (fingerprint base)
              (fingerprint (changed [:interface :guards]
                                    (vec (reverse (get-in base [:interface :guards])))))))
    (is (not= (fingerprint base)
              (fingerprint (changed [:interface :guards] [:and]))))))

(deftest type-tagging-and-length-delimiting-prevent-encoding-confusion
  (let [examples [nil false true 0 1 -1 "" "x" :x :a/x :b/x
                  "a/x" [] [:x] [:x :y] #{:x :y} [:y :x]
                  {:x nil} {:x false} {:x :y} {:y :x}
                  "a:b" "a" "b" "a;b" "a\u0000b" "a\nb"]
        subjects (map #(changed [:interface :opaque :eligibility] %) examples)
        digests (map fingerprint subjects)]
    (is (= (count examples) (count (distinct digests)))
        "Different supported values must not share a v1 encoding in this fixture")
    (is (not= (fingerprint (changed [:interface :opaque :eligibility] :a/b))
              (fingerprint (changed [:interface :opaque :eligibility] "a/b"))))
    (is (not= (fingerprint (changed [:interface :opaque :eligibility] #{:a :b}))
              (fingerprint (changed [:interface :opaque :eligibility] [:a :b]))))))

(deftest unicode-and-meaningful-whitespace-remain-sensitive
  (let [samples ["hello world" "hello  world" "hello\tworld"
                 "hello\nworld" "hello world "
                 "é" "e\u0301" "𝄞" "\ud834\udd1e" "\ud800" "\udc00"]
        ids (map #(fingerprint (changed [:interface :string-value] %)) samples)]
    ;; A literal supplementary Unicode character and the equivalent surrogate
    ;; pair are the same sequence of UTF-16 units on the JVM.
    (is (= (nth ids 7) (nth ids 8)))
    (is (= (count (set ids)) 10))
    (is (not= (nth ids 5) (nth ids 6)))
    (is (not= (nth ids 9) (nth ids 10)))))

(deftest mathematical-integer-literals-normalize-intentionally
  (let [subjects (map #(changed [:interface :number] %) [1 1N (biginteger 1)])]
    (is (apply = (map fingerprint subjects)))
    (is (true? (subject/same-interface? (first subjects) (last subjects)))))
  (is (not= (fingerprint (changed [:interface :number] -1))
            (fingerprint (changed [:interface :number] 1)))))

(defn- distinguish-integer-classes
  "Intentionally allows mathematically equal keys with different JVM types.
   Canonical encoding collapses those numeric values and MUST reject instead
   of silently losing one member/entry."
  [a b]
  (let [type-order (compare (.getName (class a)) (.getName (class b)))]
    (if (zero? type-order)
      (compare a b)
      type-order)))

(deftest canonicalization-collisions-are-rejected-not-deduplicated
  (let [ambiguous-set (into (sorted-set-by distinguish-integer-classes) [1 1N])
        ambiguous-map (into (sorted-map-by distinguish-integer-classes)
                            [[1 :one] [1N :one-bigint]])]
    (is (= 2 (count ambiguous-set)))
    (is (= 2 (count ambiguous-map)))
    (doseq [container [ambiguous-set ambiguous-map]]
      (let [result (invalid (changed [:interface :collision] container))]
        (is (= :gesso.assurance.subject/invalid-semantic-subject
               (:error/type result)))
        (is (= :normalized-collision (:reason result)))
        (is (= [:subject :interface :collision] (:at result))
            "Exact location of collision must be reported")))))

(deftest missing-and-extra-keys-must-not-accidentally-canonicalize
  (doseq [field [:kind :id :interface]]
    (let [failure (invalid (dissoc base field))]
      (is (= :wrong-keys (:reason failure)))
      (is (= #{field} (:missing failure)))
      (is (= #{} (:unexpected failure)))
      (is (= :subject (:at failure)))))
  (doseq [field [:fingerprint :valid? :certificate :proof :checking :admission]]
    (let [failure (invalid (assoc base field true))]
      (is (= :wrong-keys (:reason failure)))
      (is (= #{field} (:unexpected failure)))
      (is (= #{} (:missing failure)))))
  (is (= :wrong-keys (:reason (invalid (assoc base :fingerprint
                                             (fingerprint base)))))))

(deftest identity-input-requires-a-real-typed-subject
  (doseq [value [nil [] #{} 12 "subject" :example/claim]]
    (is (= :expected-subject-map (invalid-reason value))))
  (doseq [kind [nil :made-up :verified "operation" 1]]
    (is (= :unknown-subject-kind
           (invalid-reason (assoc base :kind kind)))))
  (doseq [id [nil :claim :unqualified "example/claim" 99]]
    (is (= :invalid-subject-id (invalid-reason (assoc base :id id)))))
  (doseq [interface [nil [] #{} {} 3 "stuff"]]
    (is (= :invalid-interface
           (invalid-reason (assoc base :interface interface))))))

(defrecord NotPlainEdn [value])

(deftest nondeclarative-values-are-not-inspected-or-executed
  (let [was-called? (atom false)
        malicious (fn [] (reset! was-called? true) true)
        unsupported [malicious '(:execute :something) 'some-symbol
                     1.5 3/4 (java.util.Date. 0)
                     (atom 10) (->NotPlainEdn 1)]]
    (doseq [value unsupported]
      (testing (str "Unsupported nested value class " (class value))
        (let [failure (invalid (changed [:interface :untrusted] value))]
          (is (= :unsupported-interface-value (:reason failure)))
          (is (= :gesso.assurance.subject/invalid-semantic-subject
                 (:error/type failure)))
          (is (= [:subject :interface :untrusted] (:at failure))))))
    (is (false? @was-called?))
    (is (= :unsupported-interface-value
           (invalid-reason (changed [:interface :untrusted] {:nested '(+ 1 2)})))))
  (is (= :unsupported-interface-value
         (invalid-reason (changed [:interface :nonedn-key]
                                  {(java.util.Date. 0) :value}))))
  (is (= :unsupported-interface-value
         (invalid-reason (changed [:interface :nonedn-key]
                                  {:x (->NotPlainEdn 2)})))))

(deftest bounded-input-complexity-fails-closed
  (let [too-deep (nth (iterate vector 0) 70)
        failure (invalid (changed [:interface :deep] too-deep))]
    (is (= :depth-limit (:reason failure)))
    (is (= 64 (:maximum failure)))
    (is (vector? (:at failure))))
  (let [overlong (apply str (repeat 600000 "a"))
        failure (invalid (changed [:interface :huge] overlong))]
    (is (= :canonical-size-limit (:reason failure)))
    (is (= 2000000 (:maximum failure)))
    (is (> (:size failure) (:maximum failure))))
  (let [many-nodes (vec (repeat 100001 nil))
        failure (invalid (changed [:interface :many-nodes] many-nodes))]
    (is (= :size-limit (:reason failure)))
    (is (= 100000 (:maximum failure)))))

(deftest invalid-subjects-cannot-be-compared-as-valid
  (let [base-id (fingerprint base)
        forged (assoc base :fingerprint base-id)
        unknown (changed [:interface :callback] (fn [] :always))]
    (is (true? (subject/same-interface? base base)))
    (is (false? (subject/same-interface? base (assoc base :id :example/other))))
    (doseq [candidate [forged unknown]]
      (is (thrown? clojure.lang.ExceptionInfo
                   (subject/same-interface? base candidate))))))

(ns gesso.live.progression-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.live.progression :as progression]))

(defn- error-data
  [f]
  (try
    (f)
    nil
    (catch #?(:clj Throwable
              :cljs :default) ex
      (ex-data ex))))

(defn- error-kind
  [f]
  (:error/kind (error-data f)))

(def basis-a
  {:tx-id 10
   :system-time "2026-08-25T20:00:00Z"})

(def basis-b
  {:tx-id 11
   :system-time "2026-08-25T20:00:01Z"})

(def basis-c
  {:tx-id 12
   :system-time "2026-08-25T20:00:02Z"})

(def basis-x
  {:tx-id 999
   :system-time "1900-01-01T00:00:00Z"})

(deftest progression-vocabulary-is-explicit
  (is (= 1 progression/progression-version))
  (is (= :gesso.live.progression/requirement
         progression/requirement-type))
  (is (= :gesso.live.progression/requirement-wire
         progression/requirement-wire-type))
  (is (= :advances progression/advance-relation)))

(deftest authoritative-bases-are-opaque-present-values
  (doseq [basis [basis-a
                 1
                 "basis-token"
                 :basis/token
                 [:xtdb "opaque"]]]
    (is (progression/basis? basis))
    (is (= basis (progression/require-basis! basis))))

  (is (false? (progression/basis? nil)))
  (is (= :invalid-basis
         (error-kind #(progression/require-basis! nil)))))

(deftest advancement-evidence-is-closed-and-endpoint-exact
  (let [witness (progression/advance basis-a basis-b)]
    (is (= {:from basis-a
            :to basis-b
            :relation :advances}
           witness))
    (is (progression/advance? witness))
    (is (progression/advances? basis-a basis-b witness))
    (is (false? (progression/advances? basis-b basis-a witness)))
    (is (false? (progression/advances? basis-a basis-c witness))))

  (doseq [witness [{:from basis-a
                    :to basis-b}
                   {:from basis-a
                    :to basis-b
                    :relation :newer}
                   {:from basis-a
                    :to basis-b
                    :relation :advances
                    :trusted? true}
                   {:from nil
                    :to basis-b
                    :relation :advances}]]
    (is (false? (progression/advance? witness)))
    (is (= :invalid-advance
           (error-kind #(progression/require-advance! witness))))))

(deftest monotone-install-never-infers-order-from-basis-shape
  (is (progression/install-allowed? nil basis-a nil))
  (is (progression/install-allowed? basis-a basis-a nil))

  (testing "a distinct basis needs an exact explicit witness"
    (is (false? (progression/install-allowed?
                 basis-a basis-b nil)))
    (is (false? (progression/install-allowed?
                 basis-a basis-b
                 (progression/advance basis-b basis-a))))
    (is (progression/install-allowed?
         basis-a basis-b
         (progression/advance basis-a basis-b))))

  (testing "apparently larger ids or later-looking timestamps confer nothing"
    (is (false? (progression/install-allowed?
                 basis-a basis-x nil)))
    (is (false? (progression/install-allowed?
                 basis-x basis-a nil)))))

(deftest requirement-construction-is-orderless-and-deduplicating
  (let [singleton (progression/requirement basis-a)
        many (progression/requirement-from-bases
              [basis-a basis-b basis-a])]
    (is (progression/requirement? singleton))
    (is (= #{basis-a}
           (progression/required-bases singleton)))
    (is (= #{basis-a basis-b}
           (progression/required-bases many)))
    (is (= many
           (progression/requirement-from-bases
            [basis-b basis-a])))
    (is (= many
           (progression/requirement-from-bases
            #{basis-a basis-b}))))

  (is (nil? (progression/normalize-requirement nil)))
  (is (= #{} (progression/required-bases nil)))
  (is (= :invalid-bases
         (error-kind
          #(progression/requirement-from-bases basis-a))))
  (is (= :empty-requirement
         (error-kind
          #(progression/requirement-from-bases []))))
  (is (= :invalid-basis
         (error-kind
          #(progression/requirement-from-bases [basis-a nil])))))

(deftest composition-is-commutative-associative-idempotent-and-nil-neutral
  (let [a (progression/requirement basis-a)
        b (progression/requirement basis-b)
        c (progression/requirement basis-c)
        ab (progression/requirement-from-bases [basis-a basis-b])
        abc (progression/requirement-from-bases
             [basis-a basis-b basis-c])]
    (is (nil? (progression/compose)))
    (is (nil? (progression/compose nil nil)))
    (is (= a (progression/compose nil a nil)))
    (is (= a (progression/compose a a)))
    (is (= ab (progression/compose a b)))
    (is (= ab (progression/compose b a)))
    (is (= abc
           (progression/compose
            (progression/compose a b)
            c)))
    (is (= abc
           (progression/compose
            a
            (progression/compose b c))))))

(deftest arrival-order-cannot-change-requirement-semantics
  (let [orders [[basis-a basis-b basis-c]
                [basis-a basis-c basis-b]
                [basis-b basis-a basis-c]
                [basis-b basis-c basis-a]
                [basis-c basis-a basis-b]
                [basis-c basis-b basis-a]]
        requirements (map progression/requirement-from-bases orders)
        composed-forward
        (apply progression/compose
               (map progression/requirement
                    [basis-a basis-b basis-c]))
        composed-reverse
        (apply progression/compose
               (map progression/requirement
                    [basis-c basis-b basis-a]))]
    (is (apply = requirements))
    (is (= composed-forward composed-reverse))
    (is (= #{basis-a basis-b basis-c}
           (progression/required-bases composed-forward)))
    (is (= (progression/requirement->wire composed-forward)
           (progression/requirement->wire composed-reverse)))))

(deftest covers-is-exact-conservative-containment-only
  (let [a (progression/requirement basis-a)
        b (progression/requirement basis-b)
        ab (progression/requirement-from-bases [basis-a basis-b])]
    (is (progression/covers? ab a))
    (is (progression/covers? ab b))
    (is (progression/covers? ab ab))
    (is (false? (progression/covers? a ab)))
    (is (false? (progression/covers? b a)))
    (is (progression/covers? nil nil))
    (is (progression/covers? ab nil))
    (is (false? (progression/covers? nil a)))))

(deftest satisfaction-requires-exact-bases-or-direct-trusted-evidence
  (let [a (progression/requirement basis-a)
        ab (progression/requirement-from-bases [basis-a basis-b])
        a->b (progression/advance basis-a basis-b)
        b->c (progression/advance basis-b basis-c)
        a->c (progression/advance basis-a basis-c)]
    (is (progression/satisfied-by? basis-a a))
    (is (false? (progression/satisfied-by? basis-b a)))
    (is (progression/satisfied-by? basis-b a [a->b]))

    (testing "every atomic requirement must be satisfied"
      (is (false? (progression/satisfied-by? basis-b ab)))
      (is (progression/satisfied-by? basis-b ab [a->b])))

    (testing "generic Live does not invent transitive closure"
      (is (false?
           (progression/satisfied-by?
            basis-c
            a
            [a->b b->c])))
      (is (progression/satisfied-by?
           basis-c
           a
           [a->c]))
      (is (false?
           (progression/satisfied-by?
            basis-c
            ab
            [a->c])))
      (is (progression/satisfied-by?
           basis-c
           ab
           [a->c b->c])))))

(deftest requirement-wire-round-trip-is-versioned-closed-and-orderless
  (let [requirement
        (progression/requirement-from-bases
         [basis-c basis-a basis-b])
        wire (progression/requirement->wire requirement)]
    (is (= progression/requirement-wire-type
           (:gesso.live.progression/type wire)))
    (is (= progression/progression-version
           (:gesso.live.progression/version wire)))
    (is (vector? (:bases wire)))
    (is (= requirement
           (progression/wire->requirement wire)))

    (testing "encoding order is stable but semantically irrelevant"
      (is (= wire
             (progression/requirement->wire
              (progression/requirement-from-bases
               [basis-a basis-b basis-c]))))))

  (is (= :unknown-fields
         (error-kind
          #(progression/wire->requirement
            {:gesso.live.progression/type
             progression/requirement-wire-type
             :gesso.live.progression/version
             progression/progression-version
             :bases [basis-a]
             :authority true}))))
  (is (= :wrong-wire-type
         (error-kind
          #(progression/wire->requirement
            {:gesso.live.progression/type :wrong/type
             :gesso.live.progression/version
             progression/progression-version
             :bases [basis-a]}))))
  (is (= :unsupported-wire-version
         (error-kind
          #(progression/wire->requirement
            {:gesso.live.progression/type
             progression/requirement-wire-type
             :gesso.live.progression/version 999
             :bases [basis-a]}))))
  (is (= :invalid-wire-bases
         (error-kind
          #(progression/wire->requirement
            {:gesso.live.progression/type
             progression/requirement-wire-type
             :gesso.live.progression/version
             progression/progression-version
             :bases #{basis-a}}))))
  (is (= :empty-requirement
         (error-kind
          #(progression/wire->requirement
            {:gesso.live.progression/type
             progression/requirement-wire-type
             :gesso.live.progression/version
             progression/progression-version
             :bases []}))))
  (is (= :invalid-basis
         (error-kind
          #(progression/wire->requirement
            {:gesso.live.progression/type
             progression/requirement-wire-type
             :gesso.live.progression/version
             progression/progression-version
             :bases [basis-a nil]})))))

(deftest normalized-requirements-are-closed
  (let [valid (progression/requirement basis-a)]
    (is (= valid
           (progression/normalize-requirement valid)))
    (is (= :invalid-requirement
           (error-kind
            #(progression/normalize-requirement basis-a))))
    (is (= :invalid-requirement
           (error-kind
            #(progression/normalize-requirement
              (assoc valid :trusted? true)))))
    (is (= :invalid-requirement
           (error-kind
            #(progression/normalize-requirement
              (assoc valid
                     :gesso.live.progression/version
                     999)))))))

(deftest explain-is-portable-and-does-not-invent-order
  (is (nil? (progression/explain nil)))
  (let [requirement
        (progression/requirement-from-bases
         [basis-a basis-b])
        explanation (progression/explain requirement)]
    (is (= progression/requirement-type
           (:gesso.live.progression/type explanation)))
    (is (= progression/progression-version
           (:gesso.live.progression/version explanation)))
    (is (= 2 (:basis-count explanation)))
    (is (= #{basis-a basis-b}
           (:bases explanation)))
    (is (= #{:gesso.live.progression/type
             :gesso.live.progression/version
             :basis-count
             :bases}
           (set (keys explanation))))))

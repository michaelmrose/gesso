(ns gesso.assurance.claim-test
  "v1210 regressions for descriptive assurance claims introduced in v1209.

   These tests check the CLOSED INPUT VOCABULARY and the distinction between
   assertions and admitted evidence.  They deliberately do not check proof
   soundness, real subject identity, assumption discharge, or deployed runtime
   correspondence; subsequent assurance modules must establish those facts."
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.assurance.claim :as claim]))

(def ^:private evidence-kinds
  #{:kernel-checked :exact-finite-checked :bounded-model-checked
    :assembly-verified :runtime-enforced :trusted :application-asserted
    :tested :unknown})

(defn- candidate
  "A complete but unadmitted assertion, not a certificate fixture."
  []
  {:subject :request/claim
   :proposition [:request/preserves-claim-exclusivity]
   :scope {:kind :operation :id :request/claim}
   :evidence {:class :tested :reference :test/claim-race}
   :assumptions #{}
   :open-obligations #{}
   :dependencies #{}
   :implementation-boundary nil})

(defn- invalid-data
  "Return the structured failure for invalid input; nil means accepted."
  [value]
  (try
    (claim/describe-claim value)
    nil
    (catch clojure.lang.ExceptionInfo e
      (ex-data e))))

(defn- invalid-class-data [kind]
  (try
    (claim/describe-evidence-class kind)
    nil
    (catch clojure.lang.ExceptionInfo e
      (ex-data e))))

(defn- assert-invalid!
  [input reason at]
  (let [failure (invalid-data input)]
    (is (map? failure) (str "Expected rejection for " (pr-str input)))
    (is (= :gesso.assurance.claim/invalid-claim-description
           (:error/type failure)))
    (is (= reason (:reason failure)))
    (when at
      (is (= at (:at failure))))))

(deftest evidence-classes-are-a-closed-descriptive-vocabulary
  (is (= evidence-kinds claim/evidence-classes))
  (is (= evidence-kinds (set (keys claim/evidence-class-descriptions))))
  (doseq [kind evidence-kinds]
    (testing (str "Published class " kind)
      (is (true? (claim/evidence-class? kind)))
      (let [result (claim/describe-evidence-class kind)]
        (is (= kind (:class result)))
        (is (string? (:description result)))
        (is (not (empty? (:description result)))))))
  (doseq [kind [nil false true :verified :proof :gesso/verified
                "kernel-checked" 3 [] {}]]
    (testing (str "Unknown or fabricated class " (pr-str kind))
      (is (false? (claim/evidence-class? kind)))
      (let [failure (invalid-class-data kind)]
        (is (= :unknown-evidence-class (:reason failure)))
        (is (= :gesso.assurance.claim/invalid-claim-description
               (:error/type failure)))))))

(deftest complete-description-is-explicitly-unadmitted
  (let [input (candidate)
        result (claim/describe-claim input)]
    (is (= #{:type :version :claim :conditional? :open?
             :checking :admission}
           (set (keys result))))
    (is (= claim/description-type (:type result)))
    (is (= :gesso.assurance.claim/unadmitted-description (:type result)))
    (is (= claim/description-version (:version result)))
    (is (= 1 (:version result)))
    (is (= input (:claim result)))
    (is (false? (:conditional? result)))
    (is (false? (:open? result)))
    (is (= :not-performed (:checking result)))
    (is (= :not-admitted (:admission result)))
    (is (not (contains? result :valid?)))
    (is (not (contains? result :certificate)))))

(deftest every-alleged-evidence-class-remains-unchecked
  ;; Even alleged :kernel-checked evidence or a plausible reference is data
  ;; until a separate checker/admission step actually verifies it.
  (doseq [kind evidence-kinds]
    (let [input (assoc (candidate) :evidence
                       {:class kind
                        :reference (when-not (= kind :unknown)
                                     :fabricated/receipt)})
          result (claim/describe-claim input)]
      (is (= kind (get-in result [:claim :evidence :class])))
      (is (= :not-performed (:checking result)))
      (is (= :not-admitted (:admission result))))))

(deftest conditionality-and-open-obligations-are-independent
  (doseq [[assumptions obligations conditional? open?]
          [[#{} #{} false false]
           [#{:external/availability} #{} true false]
           [#{} #{:execution/runtime-correspondence} false true]
           [#{:external/availability :database/atomicity}
            #{:execution/runtime-correspondence} true true]]]
    (let [result (claim/describe-claim
                  (assoc (candidate)
                         :assumptions assumptions
                         :open-obligations obligations))]
      (is (= conditional? (:conditional? result)))
      (is (= open? (:open? result)))
      (is (= assumptions (get-in result [:claim :assumptions])))
      (is (= obligations (get-in result [:claim :open-obligations])))
      (is (= :not-admitted (:admission result)))))
  (let [result (claim/describe-claim
                (assoc (candidate)
                       :dependencies #{:kernel/axiom-1 :assurance/subject-v1}
                       :implementation-boundary :xtdb/atomic-commit))]
    (is (= #{:kernel/axiom-1 :assurance/subject-v1}
           (get-in result [:claim :dependencies])))
    (is (= :xtdb/atomic-commit
           (get-in result [:claim :implementation-boundary])))
    (is (false? (:conditional? result)))))

(deftest malformed-top-level-inputs-fail-closed
  (doseq [value [nil [] :claim/other "claim" true 12]]
    (assert-invalid! value :expected-map :claim))
  (doseq [key (keys (candidate))]
    (let [failure (invalid-data (dissoc (candidate) key))]
      (is (= :wrong-keys (:reason failure)))
      (is (= :claim (:at failure)))
      (is (= #{key} (:missing failure)))
      (is (= #{} (:unexpected failure)))))
  (doseq [key [:valid? :checking :admission :verified?
               :proof/checker :type :version :certificate]]
    (let [failure (invalid-data (assoc (candidate) key true))]
      (is (= :wrong-keys (:reason failure)))
      (is (= :claim (:at failure)))
      (is (= #{key} (:unexpected failure)))
      (is (= #{} (:missing failure))))))

(deftest subject-and-proposition-must-have-valid-placeholder-forms
  (doseq [subject [nil :claim :unqualified "request/claim" [] {} 42]]
    (assert-invalid! (assoc (candidate) :subject subject)
                     :invalid-subject nil))
  (doseq [prop [nil [] '(:request/preserves-claim-exclusivity)
                :request/preserves-claim-exclusivity
                [:unnamespaced] ["request/claim"]
                [nil] [false] {}]]
    (assert-invalid! (assoc (candidate) :proposition prop)
                     :invalid-proposition-placeholder nil))
  ;; The remaining terms of a proposition are not interpreted yet: claiming
  ;; semantic validation here would be an unsound promotion of evidence.
  (let [result (claim/describe-claim
                (assoc (candidate) :proposition
                       [:request/preserves-claim-exclusivity
                        {:uninterpreted [:arbitrary "payload"]}]))]
    (is (= :not-performed (:checking result)))
    (is (= :not-admitted (:admission result)))))

(deftest scoped-claims-need-exact-shapes-and-nominal-identifiers
  (doseq [kind claim/scope-kinds]
    (let [result (claim/describe-claim
                  (assoc-in (candidate) [:scope :kind] kind))]
      (is (= kind (get-in result [:claim :scope :kind])))))
  (doseq [scope [nil "operation" []]]
    (assert-invalid! (assoc (candidate) :scope scope)
                     :expected-map :scope))
  (doseq [scope [{:kind :operation}
                 {:id :request/claim}
                 {:kind :operation :id :request/claim :valid? true}]]
    (assert-invalid! (assoc (candidate) :scope scope)
                     :wrong-keys :scope))
  (doseq [kind [nil :unknown :claimed/operation "operation" 1]]
    (assert-invalid! (assoc-in (candidate) [:scope :kind] kind)
                     :unknown-scope-kind nil))
  (doseq [id [nil :claim "request/claim" 3 []]]
    (assert-invalid! (assoc-in (candidate) [:scope :id] id)
                     :invalid-scope-id nil)))

(deftest evidence-shape-does-not-create-evidence
  (doseq [value [nil :tested [] "tested" 10]]
    (assert-invalid! (assoc (candidate) :evidence value)
                     :expected-map :evidence))
  (doseq [evidence [{:class :tested}
                    {:reference :test/claim-race}
                    {:class :tested :reference :test/claim-race
                     :valid? true}
                    {:class :kernel-checked :reference :proof/alleged
                     :certificate :proof/forged}]]
    (assert-invalid! (assoc (candidate) :evidence evidence)
                     :wrong-keys :evidence))
  (doseq [kind [nil :verified :proof "tested" 999]]
    (assert-invalid! (assoc-in (candidate) [:evidence :class] kind)
                     :unknown-evidence-class nil))
  (doseq [reference [false "proof/123" :unqualified 9 [] {}]]
    (assert-invalid! (assoc-in (candidate) [:evidence :reference] reference)
                     :invalid-evidence-reference nil))
  (assert-invalid! (assoc (candidate) :evidence
                          {:class :unknown :reference :fake/proof})
                   :unknown-with-evidence-reference nil)
  (let [result (claim/describe-claim
                (assoc (candidate) :evidence
                       {:class :unknown :reference nil}))]
    (is (= :not-admitted (:admission result)))))

(deftest assumptions-obligations-and-dependencies-must-be-named-sets
  (doseq [field [:assumptions :open-obligations :dependencies]
          value [nil [] {} :assumption/foo "assumption/foo" #{:unnamed}
                 #{nil} #{false} #{"opaque/external"} #{42}]]
    (let [failure (invalid-data (assoc (candidate) field value))]
      (is (= :expected-named-set (:reason failure))
          (str "Field " field " accepted " (pr-str value)))
      (is (= field (:at failure)))))
  (doseq [field [:assumptions :open-obligations :dependencies]]
    (let [result (claim/describe-claim
                  (assoc (candidate) field #{:example/a :example/b}))]
      (is (= #{:example/a :example/b} (get-in result [:claim field]))))))

(deftest implementation-boundary-is-explicit-but-not-verified
  (doseq [value [:xtdb/atomic-commit :browser/progression nil]]
    (let [result (claim/describe-claim
                  (assoc (candidate) :implementation-boundary value))]
      (is (= value (get-in result [:claim :implementation-boundary])))
      (is (= :not-performed (:checking result)))))
  (doseq [value [:unqualified "xtdb/atomic-commit" true {} 123 []]]
    (assert-invalid! (assoc (candidate) :implementation-boundary value)
                     :invalid-implementation-boundary nil)))

(deftest returned-descriptions-and-copies-have-no-authority
  ;; Copies, fake stamps, and serialized shapes cannot be smuggled through
  ;; describe-claim: only a fresh, closed raw claim may be described, and that
  ;; description is always unadmitted.  A future admission boundary must use
  ;; independent provenance rather than relying on this map shape.
  (let [description (claim/describe-claim (candidate))
        copied (read-string (pr-str description))
        forged (assoc description
                      :checking :passed
                      :admission :admitted
                      :valid? true)]
    (is (= description copied))
    (is (= :not-admitted (:admission description)))
    (assert-invalid! description :wrong-keys :claim)
    (assert-invalid! copied :wrong-keys :claim)
    (assert-invalid! forged :wrong-keys :claim)
    (is (= :not-admitted
           (:admission (claim/describe-claim (:claim forged)))))))

(deftest descriptions-are-pure-and-deterministic
  (let [input (candidate)
        reversed-input (into (array-map) (reverse (seq input)))
        first-result (claim/describe-claim input)]
    (is (= first-result (claim/describe-claim input)))
    (is (= first-result (claim/describe-claim reversed-input)))
    (is (= input (:claim first-result)))
    (is (= #{:assumptions :open-obligations :dependencies}
           (set (filter (comp set? input) (keys input)))))))

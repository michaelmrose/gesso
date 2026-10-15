(ns gesso.model.formal-check-test
  "Permanent JVM integration tests for Gesso's generic Recife/TLC runner.

   These tests submit *admitted Gesso transaction constructions* to
   formal-check/check-prepared!. They check real finite TLC exploration,
   including repeated sessions and source-value codec compatibility, then
   exercise fail-closed input/result handling with controlled checker stubs.

   The verified scope is the selected finite serial tx-model machine, not a
   theorem about XTDB, arbitrary application code, concurrency, or CLJS.
   Stubbed checker failures below are negative controls, NOT proof results."
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.model.command :as command]
            [gesso.model.formal-check :as check]
            [gesso.model.formal-obligation :as obligation]
            [gesso.model.formal-transition :as formal]
            [recife.core :as recife]))

(def ^:private version
  {:revision-key :revision
   :created-at-key :created-at
   :updated-at-key :updated-at})

(defn- document [id]
  {:xt/id id :revision 0 :created-at :t0 :updated-at :t0 :value :initial})

(defn- replace-plan [before table value]
  {:commands [(command/update-command
               table :replace before
               (-> before
                   (assoc :value value :updated-at :t1)
                   (update :revision inc))
               version)]
   :guards []
   :emit false})

(defn- create-plan [table id value]
  {:commands [(command/create
               table (assoc (document id) :value value) version)]
   :guards []
   :emit false})

(def ^:private alpha (document :alpha))
(def ^:private beta (document :beta))
(def ^:private alpha-target [:items :alpha])
(def ^:private beta-target [:items :beta])

(defn- competing-updates []
  (formal/prepared-subject
   {alpha-target alpha}
   [(replace-plan alpha :items :first)
    (replace-plan alpha :items :second)]))

(defn- independent-updates []
  (formal/prepared-subject
   {alpha-target alpha beta-target beta}
   [(replace-plan alpha :items :first)
    (replace-plan beta :items :second)]))

(defn- competing-creates []
  (formal/prepared-subject
   {}
   [(create-plan :items :created :first)
    (create-plan :items :created :second)]))

(defn- three-competing-updates []
  (formal/prepared-subject
   {alpha-target alpha}
   [(replace-plan alpha :items :first)
    (replace-plan alpha :items :second)
    (replace-plan alpha :items :third)]))

(defn- string-and-underscore-case []
  ;; Distinct literals must survive TLC encoding and decoding. Recife 0.22.0
  ;; otherwise normalizes underscores in keywords and does not transport raw
  ;; strings reliably. The production transition still receives real values.
  (formal/prepared-subject
   {}
   [(create-plan :formal_items :new_item "literal_payload_1")
    (create-plan :formal_items :new_item "literal_payload_2")]))

(defn- reject-kind [subject]
  (try
    (check/check-prepared! subject)
    ::not-rejected
    (catch clojure.lang.ExceptionInfo exception
      (:error/kind (ex-data exception)))))

(defn- sessions []
  ;; This is deliberately white-box: a failed or completed checker MUST not
  ;; leave the short-lived in-process codec/subject authority available.
  @(var-get (get (ns-interns 'gesso.model.formal-check) 'sessions)))

(deftest real-tlc-checks-multiple-admitted-constructions-test
  (doseq [[scenario make-subject expected-states]
          [[:competing-updates competing-updates 5]
           [:independent-updates independent-updates 5]
           [:competing-creates competing-creates 5]
           [:three-competing-updates three-competing-updates 16]
           [:strings-and-underscores string-and-underscore-case 5]]]
    (testing (name scenario)
      (let [subject (make-subject)
            result (check/check-prepared! subject)]
        (is (= :checked-finite-model (:status result)))
        (is (= :bounded-reference-model-check-not-certificate
               (:evidence-class result)))
        (is (= (:kind subject) (:subject-kind result)))
        (is (= expected-states (:distinct-states result)))
        (is (= (set obligation/obligation-ids) (:obligations result)))
        (is (= :recife-0.22.0/tlc (:checker result)))
        (is (contains? (:not-established result)
                       :actual-xtdb-implementation-refinement))
        (is (contains? (:not-established result)
                       :independent-application-certificate))
        (is (empty? (sessions)) "TLC session data must be released")))))

(deftest consecutive-checks-do-not-reuse-other-subjects-test
  (testing "alternating subjects retains distinct finite graphs and releases state"
    (let [a (competing-updates)
          b (three-competing-updates)
          checked (mapv (fn [subject]
                          (let [result (check/check-prepared! subject)]
                            (is (empty? (sessions)))
                            (:distinct-states result)))
                        [a b a])]
      (is (= [5 16 5] checked)))))

(deftest tampered-subjects-fail-before-starting-tlc-test
  (let [original (competing-updates)
        calls (atom 0)]
    (with-redefs [recife/run-model (fn [& _] (swap! calls inc)
                                    (throw (ex-info "TLC should not execute" {})))]
      (testing "canonical subject admission is not an assertion on a supplied tag"
        (is (= :invalid-subject (reject-kind (dissoc original :kind))))
        (is (= :modified-subject (reject-kind (assoc original :scope {}))))
        (is (= :modified-subject (reject-kind (update original :actions conj [:fake 3]))))
        (is (= :modified-subject
               (reject-kind (assoc-in original
                                      [:initial-state ::formal/pending] [0 0]))))
        ;; An inconsistent document identity is rejected by tx-model's input
        ;; validation; a *different but well-formed* prestate is not evidence
        ;; forgery that this local checker can authenticate.
        (is (thrown? clojure.lang.ExceptionInfo
                     (check/check-prepared!
                      (assoc-in original
                                [:initial-state ::formal/documents alpha-target :xt/id]
                                :incorrect-id)))))
      (is (zero? @calls) "Malformed subjects must not invoke the checker")
      (is (empty? (sessions))))))

(deftest checker-inconclusive-and-counterexample-results-fail-closed-test
  (let [subject (competing-updates)
        returned [{:trace :ok :trace-info nil :distinct-states 0}
                  {:trace :ok :trace-info {:warning :not-verified} :distinct-states 5}
                  {:trace [{:state :bad}]
                   :trace-info {:violation {:type :invariant}}
                   :distinct-states 3}
                  nil]]
    (doseq [raw returned]
      (testing (str "unacceptable Recife result " (pr-str raw))
        (with-redefs [recife/run-model (fn [& _] raw)]
          (is (= :model-check-failed (reject-kind subject)))
          (is (empty? (sessions))))))
    (testing "a throwing checker still releases its session"
      (with-redefs [recife/run-model (fn [& _]
                                      (throw (ex-info "TLC crashed" {:tester true})))]
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"TLC crashed"
                              (check/check-prepared! subject)))
        (is (empty? (sessions)))))
    (testing "an IDeref whose evaluation fails also releases its session"
      (with-redefs [recife/run-model
                    (fn [& _] (delay (throw (ex-info "TLC future failed" {}))))]
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"TLC future failed"
                              (check/check-prepared! subject)))
        (is (empty? (sessions)))))))

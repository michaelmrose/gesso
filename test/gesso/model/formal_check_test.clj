(ns gesso.model.formal-check-test
  "Permanent JVM integration tests for Gesso's generic Recife/TLC runner.

   These tests submit *admitted Gesso transaction constructions* to
   formal-check/check-prepared!. They check real finite TLC exploration,
   including repeated sessions and source-value codec compatibility, then
   exercise fail-closed input/result handling with controlled checker stubs.
   Nullable-value cases cover real TLC transport and a private codec
   round-trip; they are tests of the selected finite model, not XTDB.

   The verified scope is the selected finite serial tx-model machine, not a
   theorem about XTDB, arbitrary application code, concurrency, or CLJS.
   Stubbed checker failures below are negative controls, NOT proof results."
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.model.command :as command]
            [gesso.model.formal-check :as check]
            [gesso.model.formal-family :as family]
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

(deftest structurally-incomplete-checker-responses-fail-closed-test
  ;; These responses are deliberately substituted at the Recife boundary.
  ;; This is a validator/cleanup regression, NOT a model-checking proof.
  ;; The separate real-TLC tests above establish the valid execution path.
  (let [subject (competing-updates)
        complete {:trace :ok :trace-info nil :distinct-states 5}
        malformed
        [[:missing-trace (dissoc complete :trace)]
         [:missing-trace-info (dissoc complete :trace-info)]
         [:missing-distinct-states (dissoc complete :distinct-states)]
         [:missing-all-required-fields {:elapsed-ms 1}]
         [:nil-response nil]
         [:non-map-response []]
         [:nil-trace (assoc complete :trace nil)]
         [:inconclusive-trace (assoc complete :trace :unknown)]
         [:empty-trace (assoc complete :trace [])]
         [:non-nil-trace-info (assoc complete :trace-info {})]
         [:false-trace-info (assoc complete :trace-info false)]
         [:nil-distinct-states (assoc complete :distinct-states nil)]
         [:zero-distinct-states (assoc complete :distinct-states 0)]
         [:negative-distinct-states (assoc complete :distinct-states -2)]
         [:fractional-distinct-states (assoc complete :distinct-states 5.5)]
         [:string-distinct-states (assoc complete :distinct-states "5")]
         [:boolean-distinct-states (assoc complete :distinct-states true)]]]
    (testing "the complete, explicit response shape is recognized by the wrapper"
      ;; Do not interpret this stub as proving the underlying model safe:
      ;; it only confirms the response-shape validator's positive control.
      (with-redefs [recife/run-model (fn [& _] complete)]
        (let [result (check/check-prepared! subject)]
          (is (= :checked-finite-model (:status result)))
          (is (= :bounded-reference-model-check-not-certificate
                 (:evidence-class result)))
          (is (empty? (sessions))))))
    (doseq [[variant response] malformed]
      (testing (name variant)
        (with-redefs [recife/run-model (fn [& _] response)]
          (is (= :model-check-failed (reject-kind subject)))
          (is (empty? (sessions)) "Failure must release the checker session"))))
    (testing "an incomplete dereferenced checker result is rejected as well"
      (with-redefs [recife/run-model
                    (fn [& _] (delay (dissoc complete :trace-info)))]
        (is (= :model-check-failed (reject-kind subject)))
        (is (empty? (sessions)))))))


;; Nullable values must remain ordinary domain data. In particular, an
;; unclaimed owner is nil, NOT a keyword chosen to appease TLC's value parser.
;; This family test models both helpers competing for the same document.
(defn- nullable-owner-family []
  (let [before (assoc (document :claimable) :owner nil)
        initial {[:items :claimable] before}
        calls (atom [])
        families [{:id :example/claim
                   :principals [:helper/a :helper/b]
                   :arguments [{:request-id :claimable}]
                   :enabled? (fn [state principal args]
                               (swap! calls conj [:enabled principal args])
                               (and (= initial state)
                                    (= :claimable (:request-id args))
                                    (nil? (:owner (get state [:items :claimable])))))
                   :prepare (fn [state principal args]
                              (swap! calls conj [:prepare principal args])
                              (let [previous (get state [:items :claimable])]
                                {:commands [(command/update-command
                                             :items :claim previous
                                             (-> previous
                                                 (assoc :owner principal :updated-at :t1)
                                                 (update :revision inc))
                                             version)]
                                 :guards [] :emit false}))}]]
    {:built (family/prepared-competition initial families)
     :calls calls}))

(deftest nullable-owner-in-application-family-survives-real-tlc-test
  (let [{:keys [built calls]} (nullable-owner-family)
        subject (:subject built)
        initial (formal/initial-state subject)
        before (::formal/documents initial)
        first-step (formal/transition subject initial (first (:actions subject)))
        result (check/check-prepared! subject)]
    (is (= 2 (:choice-count built)))
    (is (= 2 (count (:plans subject))))
    (is (= 4 (count @calls)) "Every family choice is enabled and prepared once")
    (is (nil? (get-in before [[:items :claimable] :owner])))
    (is (= :helper/a
           (get-in first-step [::formal/documents [:items :claimable] :owner])))
    (is (= :checked-finite-model (:status result)))
    (is (= 5 (:distinct-states result)))
    (is (= (set obligation/obligation-ids) (:obligations result)))
    (is (contains? (:not-established result)
                   :complete-application-operation-enumeration))
    (is (empty? (sessions)))))

(deftest nil-in-planned-creation-and-literal-codec-collision-survives-tlc-test
  ;; Nil and the codec's otherwise-first candidate token both occur in the
  ;; admitted plans. Neither may be conflated with the other by TLC transport.
  (let [subject (formal/prepared-subject
                 {}
                 [(create-plan :items :created nil)
                  (create-plan :items :created :gessoformalcodec/v0)])
        result (check/check-prepared! subject)]
    (is (= :checked-finite-model (:status result)))
    (is (= 5 (:distinct-states result)))
    (is (= (set obligation/obligation-ids) (:obligations result)))
    (is (empty? (sessions)))))

(deftest nil-created-only-by-reference-transition-is-reserved-by-codec-test
  ;; In this input neither the initial documents nor the admitted plans hold
  ;; nil. A successful reference transition *does* produce a nil-valued
  ;; :failed-precondition observation. The codec must already represent it.
  (let [subject (competing-updates)
        state (formal/transition subject (formal/initial-state subject)
                                 (first (:actions subject)))
        observation (first (::formal/observations state))
        codec ((var-get (ns-resolve 'gesso.model.formal-check 'codec-for))
               subject)
        encoded ((:encode codec) observation)]
    (is (nil? (:failed-precondition observation)))
    (is (contains? observation :failed-precondition))
    (is (some? (:failed-precondition encoded))
        "TLC must never receive a raw nil inside a nested observation")
    (is (= observation ((:decode codec) encoded)))
    (is (= nil ((:decode codec) ((:encode codec) nil))))))

(deftest nested-nullable-token-codec-is-reversible-and-fails-closed-test
  ;; These assertions exercise a private codec boundary, not a mocked TLC
  ;; proof. Actual model checking is exercised by the two tests above.
  (let [nested {:owner nil
                :value "payload_with_underscores"
                :literal-keyword :gessoformalcodec/v0
                :under_score :some_namespace/key_with_underscores
                :nested [{nil {:child nil}} nil]}
        subject (formal/prepared-subject
                 {} [(create-plan :items :nested nested)])
        codec ((var-get (ns-resolve 'gesso.model.formal-check 'codec-for))
               subject)
        encode (:encode codec)
        decode (:decode codec)
        encoded (encode nested)
        nil-token (encode nil)]
    (is (keyword? nil-token))
    (is (not= nil-token :gessoformalcodec/v0))
    (is (not= nil-token (encode :gessoformalcodec/v0)))
    (is (not= nested encoded))
    (is (= nested (decode encoded)))
    (is (= [nil nil] (decode (encode [nil nil]))))
    (is (not (contains? (get-in encoded [:nested 0]) nil))
        "A nil map key must not survive encoding as raw nil")
    (is (contains? (get-in encoded [:nested 0]) nil-token))
    (is (= :unadmitted-string
           (try (encode "string_not_admitted_by_subject")
                ::not-rejected
                (catch clojure.lang.ExceptionInfo ex
                  (:error/kind (ex-data ex))))))
    (is (empty? (sessions)))))


;; ---------------------------------------------------------------------------
;; v956: diagnostics are NOT certificates. These controls substitute results at
;; the Recife boundary to test classification, redaction and local inspection.
;; They do not claim any additional TLC exploration or XTDB correspondence.
;; ---------------------------------------------------------------------------

(defn- captured-formal-failure [subject raw]
  (with-redefs [recife/run-model (fn [& _] raw)]
    (try
      (check/check-prepared! subject)
      ::unexpected-success
      (catch clojure.lang.ExceptionInfo e
        e))))

(defn- failure-records []
  @(var-get (get (ns-interns 'gesso.model.formal-check) 'failure-records)))

(def ^:private sensitive-marker "DO_NOT_EXPOSE_PRIVATE_MODEL_DATA_v956")

(defn- controlled-trace [state-count]
  ;; Model-state and business-value fields are intentionally confidential.
  ;; Only a bounded prefix of numeric state/plan positions and enumerated
  ;; observation statuses is allowed to leave the local diagnostic registry.
  (mapv (fn [i]
          [i {::formal/documents
              {[:private-table :private-id]
               {:xt/id :private-id :secret sensitive-marker}}
              ::formal/observations
              (vec (concat [{:plan-index 0
                             :status :committed
                             :secret sensitive-marker
                             :target [:private-table :private-id]}
                            {:plan-index 999 :status :sensitive-state
                             :secret sensitive-marker}]
                           (repeat 6 {:plan-index -1
                                      :status sensitive-marker})))}])
        (range state-count)))

(deftest structured-invariant-counterexample-preserves-private-trace-test
  (let [subject (competing-updates)
        raw {:trace (controlled-trace 35)
             :trace-info {:violation
                          {:type :invariant
                           :name :gesso.model.formal-check/outcome-check-consistency
                           :private-info sensitive-marker}}
             :distinct-states 35}
        error (captured-formal-failure subject raw)]
    (is (instance? clojure.lang.ExceptionInfo error))
    (when (instance? clojure.lang.ExceptionInfo error)
      (let [data (ex-data error)
            summary (:diagnostic/summary data)
            handle (:diagnostic/handle data)
            stored (check/inspect-failure handle)
            prefix (:transition-prefix summary)]
        (is (= :model-check-failed (:error/kind data)))
        (is (= :invariant-counterexample (:failure/class data)))
        (is (= :invariant-counterexample (:failure/class summary)))
        (is (= :invariant (:violation/type summary)))
        (is (= :outcome-check-consistency (:failed-obligation summary)))
        (is (= (:kind subject) (:subject-kind summary)))
        (is (= 2 (:selected-plan-count summary)))
        (is (= :prepared-transaction-finite-model (:source-boundary summary)))
        (is (= :recife-0.22.0/tlc (:checker summary)))
        (is (= 35 (:distinct-states summary)))
        (is (contains? (:not-established summary)
                       :actual-xtdb-implementation-refinement))
        (is (instance? java.util.UUID handle))
        (is (= 32 (count prefix)) "Only the first 32 states are summarized")
        (is (= (range 32) (mapv :state-index prefix)))
        (is (every? #(= 6 (count (:observations %))) prefix)
            "Only the first six observations of each state are summarized")
        (is (= {:plan-index 0 :status :committed}
               (first (:observations (second prefix)))))
        (is (= {} (second (:observations (second prefix))))
            "Out-of-range or unrecognized status is not trusted")
        (is (not (.contains (pr-str data) sensitive-marker))
            "Neither nested exception data nor redacted prefix expose domain values")
        (is (not (.contains (pr-str data) "private-table")))
        (is (not (contains? data :trace)))
        (is (not (contains? data :raw-result)))
        (is (not (contains? data :subject)))
        (is (= raw (:raw-result stored)))
        (is (= subject (:subject stored)))
        (is (= summary (:summary stored)))
        (is (= (mapv identity (:trace raw)) (:decoded-trace stored)))
        (is (= nil (check/inspect-failure (str handle))))
        (is (= nil (check/inspect-failure :not-a-handle)))
        (is (empty? (sessions)))))))

(deftest unknown-or-untrusted-obligation-names-are-never-attributed-test
  (let [subject (competing-updates)]
    (doseq [name [:unrelated-framework-obligation
                  :gesso.model.formal-check/unknown-obligation
                  "gesso.model.formal-check/outcome-check-consistency"
                  nil]]
      (testing (str "unrecognized invariant name " (pr-str name))
        (let [raw {:trace [[0 {::formal/observations []}]]
                   :trace-info {:violation {:type :invariant :name name}}
                   :distinct-states 1}
              error (captured-formal-failure subject raw)
              summary (:diagnostic/summary (ex-data error))]
          (is (= :invariant-counterexample (:failure/class (ex-data error))))
          (is (not (contains? summary :failed-obligation)))
          (is (= raw (:raw-result
                      (check/inspect-failure
                       (:diagnostic/handle (ex-data error)))))))))
    (is (empty? (sessions)))))

(deftest checker-result-failure-classes-remain-distinct-test
  (let [subject (competing-updates)
        cases [[:model-counterexample
                {:trace [[0 {::formal/observations []}]]
                 :trace-info {:violation {:type :liveness}}
                 :distinct-states 1}]
               [:invariant-counterexample
                {:trace :violation-detected
                 :trace-info {:violation {:type :invariant
                                          :name :gesso.model.formal-check/observation-chain}}
                 :distinct-states 2}]
               [:backend-error
                {:trace :error :trace-info {:message sensitive-marker}
                 :distinct-states 0}]
               [:inconclusive-checker-result
                {:trace :ok :trace-info nil :distinct-states 0}]
               [:inconclusive-checker-result
                {:trace :ok :trace-info {:warning :incomplete}
                 :distinct-states 5}]
               [:unrecognized-checker-result
                {:trace :unrecognized :trace-info nil :distinct-states 5}]
               [:malformed-checker-result nil]
               [:malformed-checker-result {:trace :ok :trace-info nil}]
               [:malformed-checker-result [:not-a-checker-result]]]]
    (doseq [[expected raw] cases]
      (testing (str expected " " (pr-str (:trace raw)))
        (let [error (captured-formal-failure subject raw)
              data (ex-data error)
              handle (:diagnostic/handle data)]
          (is (= :model-check-failed (:error/kind data)))
          (is (= expected (:failure/class data)))
          (is (= expected (get-in data [:diagnostic/summary :failure/class])))
          (is (= raw (:raw-result (check/inspect-failure handle))))
          (is (not (.contains (pr-str data) sensitive-marker))))))
    (is (empty? (sessions)))))

(deftest backend-exceptions-keep-causes-private-and-classified-test
  (let [subject (competing-updates)
        cases [[:backend-exception
                (ex-info "Internal TLC failure" {:private sensitive-marker})]
               [:checker-timeout
                (java.util.concurrent.TimeoutException. "TLC timed out")]]]
    (doseq [[expected cause] cases]
      (testing (name expected)
        (with-redefs [recife/run-model (fn [& _] (throw cause))]
          (let [error (try (check/check-prepared! subject)
                           ::unexpected-success
                           (catch clojure.lang.ExceptionInfo e e))
                data (ex-data error)
                handle (:diagnostic/handle data)
                local (check/inspect-failure handle)]
            (is (= :model-check-failed (:error/kind data)))
            (is (= expected (:failure/class data)))
            (is (= expected (get-in data [:diagnostic/summary :failure/class])))
            (is (= :recife-tlc-execution
                   (get-in data [:diagnostic/summary :source-boundary])))
            (is (= cause (.getCause error)))
            (is (= cause (:backend-exception local)))
            (is (nil? (:raw-result local)))
            (is (not (.contains (pr-str data) sensitive-marker)))
            (is (empty? (sessions)))))))
    (testing "a dereferenced asynchronous failure is classified identically"
      (with-redefs [recife/run-model
                    (fn [& _]
                      (delay (throw (java.util.concurrent.TimeoutException.
                                     "Deferred TLC timeout"))))]
        (let [error (try (check/check-prepared! subject)
                         ::unexpected-success
                         (catch clojure.lang.ExceptionInfo e e))]
          (is (= :checker-timeout (:failure/class (ex-data error))))
          (is (empty? (sessions))))))))

(deftest diagnostic-registry-is-bounded-and-evicts-expired-handles-test
  (let [subject (competing-updates)
        raw {:trace :error :trace-info nil :distinct-states 0}
        handles (mapv (fn [_]
                        (let [error (captured-formal-failure subject raw)]
                          (:diagnostic/handle (ex-data error))))
                      (range 19))
        {:keys [order entries]} (failure-records)]
    (is (= (count handles) (count (distinct handles))))
    (is (= 16 (count order)) "The bounded process-local registry keeps 16 records")
    (is (= (set order) (set (keys entries))))
    (is (= (subvec handles 3) order))
    (is (every? nil? (map check/inspect-failure (take 3 handles)))
        "Old diagnostic handles expire rather than exposing stale cases")
    (is (every? some? (map check/inspect-failure (drop 3 handles))))
    (is (empty? (sessions)))))

(deftest valid-check-does-not-create-failure-handle-test
  (let [subject (competing-updates)
        successful {:trace :ok :trace-info nil :distinct-states 5}
        before (failure-records)]
    (with-redefs [recife/run-model (fn [& _] successful)]
      ;; This only checks result-shape validation, not actual TLC soundness.
      (let [result (check/check-prepared! subject)]
        (is (= :checked-finite-model (:status result)))
        (is (= :bounded-reference-model-check-not-certificate
               (:evidence-class result)))
        (is (= (set obligation/obligation-ids) (:obligations result)))
        (is (not (contains? result :diagnostic/handle)))
        (is (= before (failure-records)))
        (is (empty? (sessions)))))))

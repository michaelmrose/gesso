(ns gesso.model.tx-refinement-subject-test
  "Adversarial regressions for the v1033 *unattested* XTDB refinement subject.

   Subjects name one admitted plan, selected finite state and DECLARED context.
   These tests do not authenticate claimed digests, prove complete policy/writer
   closure, verify evidence, or discharge XTDB backend premises."
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.model.command :as command]
            [gesso.model.tx-refinement-premises :as premises]
            [gesso.model.tx-refinement-subject :as subject])
  (:import [java.math BigDecimal]
           [java.time Instant]
           [java.util UUID]))

(def ^:private version
  {:revision-key :revision
   :created-at-key :created-at
   :updated-at-key :updated-at})

(def ^:private request-id "private-subject-request-1034")
(def ^:private membership-id "private-subject-membership-1034")
(def ^:private organization-id "private-subject-organization-1034")

(def ^:private request
  {:xt/id request-id :revision 1 :created-at "t0" :updated-at "t1"
   :owner "private-helper-a" :details {:tag :internal :items [1 2]}})
(def ^:private membership
  {:xt/id membership-id :revision 3 :created-at "t0" :updated-at "t1"
   :active? true})
(def ^:private organization
  {:xt/id organization-id :revision 2 :created-at "t0" :updated-at "t1"
   :policy "private-policy"})

(def ^:private create-document
  {:xt/id request-id :revision 0 :created-at "t0" :updated-at "t0"
   :details {:one [1 2] :two #{:alpha :beta}}})

(def ^:private create-plan
  {:commands [(command/create :requests create-document version)]
   :guards [] :assertions [] :emit false})

(def ^:private next-request
  (assoc request :revision 2 :updated-at "t2" :owner "private-helper-b"))
(def ^:private guarded-plan
  {:commands [(command/update-command :requests :claim request next-request version)]
   :guards [(command/guard :memberships membership version)
            (command/guard :organizations organization version)]
   :assertions [] :emit false})
(def ^:private guarded-state
  {[:requests request-id] request
   [:memberships membership-id] membership
   [:organizations organization-id] organization})

(def ^:private digest-a (apply str (repeat 64 "a")))
(def ^:private digest-b (apply str (repeat 64 "b")))
(def ^:private environment
  {:backend {:product :xtdb :version "2.2.0-beta1"
             :build-sha256 digest-a
             :settings {:clock :system :feature-flags #{:stable :checked}
                        :nested {:alpha [1 2] :beta true}}}
   :gesso {:source-sha256 digest-a}
   :application {:id :humanhelp :operation :create-request
                 :source-sha256 digest-a
                 :dependency-targets [[:requests request-id]
                                      [:memberships membership-id]
                                      [:organizations organization-id]]
                 :writer-ids [:humanhelp/requests :humanhelp/claims]
                 :policy-rule-ids [:humanhelp/active-member
                                   :humanhelp/organization-policy]}})

(defn- subject-of
  ([] (subject/derive-subject {} create-plan environment))
  ([state plan env] (subject/derive-subject state plan env)))

(defn- identifier [state plan env]
  (:subject-sha256 (subject-of state plan env)))

(defn- error-reason [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo e
         [(:error/type (ex-data e)) (:error/reason (ex-data e))])))

(defn- invalid? [f]
  (= ::subject/invalid-subject (first (error-reason f))))

(defn- replace-open-premises [transform]
  (let [original premises/derive-obligation-ledger]
    (with-redefs [premises/derive-obligation-ledger
                  (fn [state plan]
                    (transform (original state plan)))]
      (subject-of))))

(deftest identity-scope-excludes-attestation-and-certification-test
  (let [r (subject-of)]
    (is (= :gesso.model.tx/serial-refinement-instance-subject-v1 (:identity r)))
    (is (= subject/subject-scope (:scope r)))
    (is (= :gesso.model.tx/serial-refinement-premise-ledger-v1
           (:upstream subject/subject-scope)))
    (is (= :one-selected-abstract-prestate-and-plan (:binding-granularity r)))
    (is (= :bound-declarations-unattested (:status r)))
    (is (re-matches #"[0-9a-f]{64}" (:subject-sha256 r)))
    (doseq [field [:catalog-bound? :plan-and-prestate-bound?
                   :backend-and-source-descriptors-bound?
                   :declared-application-footprint-bound?
                   :canonical-targets-covered?]]
      (is (true? (get r field)) (name field)))
    (doseq [field [:descriptors-authenticated?
                   :application-dependencies-proved-complete?
                   :application-writers-proved-closed?
                   :physical-prestate-attested?
                   :evidence-verified?
                   :general-refinement-proved?
                   :application-certified?]]
      (is (false? (get r field)) (name field)))
    (is (= #{} (:premises-discharged r)))
    (is (contains? (:not-established subject/subject-scope)
                   :confidentiality-of-low-entropy-digest-input))))

(deftest subject-is-reproducible-across-repeated-calls-test
  (let [one (subject-of) two (subject-of)]
    (is (= one two))
    (is (= (:subject-sha256 one) (:subject-sha256 two)))))

(deftest nested-map-set-and-descriptor-map-order-is-irrelevant-test
  (let [a (identifier {} create-plan environment)
        differently-ordered (-> environment
                                (assoc-in [:backend :settings]
                                          (array-map :nested (array-map :beta true :alpha [1 2])
                                                     :feature-flags #{:checked :stable}
                                                     :clock :system)))
        reordered-doc (assoc create-document :details
                             (array-map :two #{:beta :alpha} :one [1 2]))
        reordered-plan (assoc create-plan :commands
                              [(command/create :requests reordered-doc version)])]
    (is (= a (identifier {} create-plan differently-ordered)))
    (is (= a (identifier {} reordered-plan environment)))))

(deftest declarative-footprint-order-does-not-change-identity-test
  (let [a (identifier {} create-plan environment)]
    (doseq [field [:dependency-targets :writer-ids :policy-rule-ids]]
      (is (= a (identifier {} create-plan
                           (update-in environment [:application field]
                                      (comp vec reverse))))
          (name field)))))

(deftest semantic-plan-changes-change-identity-test
  (let [a (identifier {} create-plan environment)]
    (doseq [doc [(assoc create-document :extra :something)
                 (update-in create-document [:details :one] conj 3)
                 (assoc create-document :details {:one [2 1] :two #{:alpha :beta}})
                 (assoc create-document :status nil)]]
      (is (not= a (identifier {}
                              (assoc create-plan :commands
                                     [(command/create :requests doc version)])
                              environment))))))

(deftest selected-prestate-changes-instance-identity-test
  (let [a (identifier {} create-plan environment)
        present {[:requests request-id] create-document}
        b (identifier present create-plan environment)]
    (is (not= a b))
    ;; A legitimately rejected create still has a valid *local* argument.
    (is (= :bound-declarations-unattested
           (:status (subject-of present create-plan environment))))
    (is (= b (identifier present create-plan environment)))
    (is (not= b (identifier {[:requests request-id]
                            (assoc create-document :details {:private :changed})}
                           create-plan environment)))))

(deftest guarded-claim-binds-all-read-and-write-targets-test
  (let [a (identifier guarded-state guarded-plan environment)]
    (is (re-matches #"[0-9a-f]{64}" a))
    (is (not= a (identifier (assoc-in guarded-state
                                     [[:memberships membership-id] :revision] 4)
                            guarded-plan environment)))
    (is (not= a (identifier guarded-state
                            (assoc guarded-plan :commands
                                   [(command/update-command :requests :claim request
                                                            (assoc next-request :owner :other)
                                                            version)])
                            environment)))
    (doseq [target [[:requests request-id]
                    [:memberships membership-id]
                    [:organizations organization-id]]]
      (is (= [::subject/invalid-subject :incomplete-subject-contract]
             (error-reason #(subject-of guarded-state guarded-plan
                                        (update-in environment
                                                   [:application :dependency-targets]
                                                   (fn [v] (vec (remove #{target} v)))))))))))

(deftest source-and-backend-descriptors-are-binding-inputs-test
  (let [a (identifier {} create-plan environment)
        variations [(assoc-in environment [:backend :version] "other")
                    (assoc-in environment [:backend :build-sha256] digest-b)
                    (assoc-in environment [:backend :settings :clock] :fixed)
                    (assoc-in environment [:gesso :source-sha256] digest-b)
                    (assoc-in environment [:application :source-sha256] digest-b)
                    (assoc-in environment [:application :id] :different-app)
                    (assoc-in environment [:application :operation] :cancel-request)
                    (update-in environment [:application :dependency-targets]
                               conj [:policies :additional])
                    (update-in environment [:application :writer-ids]
                               conj :humanhelp/other-writer)
                    (update-in environment [:application :policy-rule-ids]
                               conj :humanhelp/other-policy)]]
    (doseq [e variations]
      (is (not= a (identifier {} create-plan e))))))

(deftest typed-canonical-encoding-distinguishes-values-test
  (let [a (identifier {} create-plan environment)
        with-value (fn [v]
                     (identifier {} create-plan
                                 (assoc-in environment [:backend :settings :typed] v)))]
    (is (not= (with-value :pending) (with-value "pending")))
    (is (not= (with-value [1 2]) (with-value '(1 2))))
    (is (not= (with-value #{1 2}) (with-value [1 2])))
    (is (not= (with-value 1) (with-value 1N)))
    (is (not= (with-value 1) (with-value 1.0)))
    (is (not= (with-value nil) (with-value false)))
    (is (not= (with-value (BigDecimal. "1.0"))
              (with-value (BigDecimal. "1.00"))))
    (is (not= (with-value (UUID/fromString "00000000-0000-0000-0000-000000000001"))
              (with-value "00000000-0000-0000-0000-000000000001")))
    (is (not= (with-value (Instant/parse "2026-10-16T00:00:00Z"))
              (with-value "2026-10-16T00:00:00Z")))
    (is (not= a (with-value :new-key)))))

(deftest invalid-environment-and-digest-descriptors-fail-closed-test
  (doseq [e [(dissoc environment :backend)
             (assoc environment :verified? true)
             (assoc-in environment [:backend :product] :other)
             (assoc-in environment [:backend :version] "  ")
             (assoc-in environment [:backend :settings] [])
             (assoc-in environment [:backend :build-sha256] (str "A" (subs digest-a 1)))
             (assoc-in environment [:backend :build-sha256] "short")
             (assoc-in environment [:gesso :source-sha256] nil)
             (assoc-in environment [:application :operation] "claim")
             (assoc-in environment [:application :id] nil)
             (assoc-in environment [:application :writer-ids] '(:one :two))
             (assoc-in environment [:application :writer-ids] [:one :one])
             (assoc-in environment [:application :policy-rule-ids] [:one "not-keyword"])
             (assoc-in environment [:application :dependency-targets]
                       [[:requests request-id] [:requests request-id]])
             (assoc-in environment [:application :dependency-targets]
                       [[:requests nil]])
             (assoc-in environment [:application :dependency-targets]
                       [[:requests request-id] :bad-target])]]
    (is (= [::subject/invalid-subject :invalid-environment]
           (error-reason #(subject-of {} create-plan e))))))

(deftest extra-declaration-keys-cannot-smuggle-assurance-claims-test
  (doseq [path [[:backend :authenticated?]
                [:gesso :verified?]
                [:application :writers-proved-complete?]]]
    (is (= [::subject/invalid-subject :invalid-environment]
           (error-reason #(subject-of {} create-plan
                                      (assoc-in environment path true)))))))

(deftest no-unsupported-or-nondeterministic-values-are-hashed-test
  (doseq [bad [inc (atom 4) (Object.) (lazy-seq [1 2])
               Double/NaN Double/POSITIVE_INFINITY
               {:nested {:arbitrary (Object.)}}
               ]]
    (is (= [::subject/invalid-subject :unsupported-content-type]
           (error-reason #(subject-of {} create-plan
                                      (assoc-in environment [:backend :settings :unsupported] bad))))
           (str "Unexpectedly admitted " (.getName (class bad))))))

(deftest mutable-java-number-must-not-enter-subject-hash-test
  ;; Mutable java.lang.Number implementations are not reproducible subject
  ;; inputs. v1035 rejects them with an exact-class scalar allowlist.
  (let [mutable (java.util.concurrent.atomic.AtomicLong. 3)]
    (is (= [::subject/invalid-subject :unsupported-content-type]
           (error-reason #(subject-of {} create-plan
                                      (assoc-in environment
                                                [:backend :settings :mutable-number]
                                                mutable)))))))


;; v1036: permanent allowlist coverage for the v1035 canonical encoder.
;; All probes vary a backend *declaration*, not the production XTDB backend.
;; Successful digests bind caller-provided data; they do not attest its origin.
(defn- scalar-identity [value]
  (identifier {} create-plan
              (assoc-in environment [:backend :settings :scalar-audit] value)))

(defn- scalar-rejection [value]
  (error-reason #(scalar-identity value)))

(deftest immutable-numeric-allowlist-hashes-reproducibly-test
  (let [accepted [(byte 7) (short 7) (int 7) (long 7)
                  (java.math.BigInteger. "700000000000000000000001")
                  (bigint 700000000000000000000001)
                  (/ 2 3) (java.math.BigDecimal. "7.00")
                  (float 7.25) (double -0.0)
                  Double/MIN_VALUE Float/MIN_VALUE
                  Long/MAX_VALUE]]
    (doseq [value accepted]
      (let [first-id (scalar-identity value)]
        (is (re-matches #"[0-9a-f]{64}" first-id)
            (str "Accepted immutable numeric class: " (class value)))
        (is (= first-id (scalar-identity value))
            (str "Non-reproducible immutable numeric class: " (class value)))))))

(deftest numeric-scalar-class-and-representation-remain-bound-test
  ;; Numeric equality must not erase declared type or BigDecimal scale.
  (doseq [[left right] [[(byte 1) (short 1)]
                        [(short 1) (int 1)]
                        [(int 1) (long 1)]
                        [(long 1) (java.math.BigInteger. "1")]
                        [(java.math.BigInteger. "1") (bigint 1)]
                        [(long 1) (float 1.0)]
                        [(float 1.0) (double 1.0)]
                        [(double 1.0) (java.math.BigDecimal. "1.0")]
                        [(java.math.BigDecimal. "1.0")
                         (java.math.BigDecimal. "1.00")]
                        [(double 0.0) (double -0.0)]]]
    (is (not= (scalar-identity left) (scalar-identity right))
        (str (class left) " versus " (class right)
             " must retain typed or represented distinction"))))

(deftest unknown-and-mutable-numeric-subclasses-are-rejected-test
  (let [unknown-number (proxy [Number] []
                         (intValue [] 9) (longValue [] 9)
                         (floatValue [] (float 9))
                         (doubleValue [] (double 9)))]
    (doseq [value [(java.util.concurrent.atomic.AtomicLong. 3)
                   (java.util.concurrent.atomic.AtomicInteger. 4)
                   (java.util.concurrent.atomic.LongAdder.)
                   (java.util.concurrent.atomic.DoubleAdder.)
                   unknown-number]]
      (is (= [::subject/invalid-subject :unsupported-content-type]
             (scalar-rejection value))
          (str "Open Number subclass must not be admitted: " (class value))))))

(deftest legacy-mutable-date-types-are-rejected-test
  (doseq [value [(java.util.Date. 1760595600000)
                 (java.sql.Timestamp. 1760595600000)
                 (java.sql.Date. 1760572800000)
                 (java.sql.Time. 3600000)
                 (java.util.GregorianCalendar.)]]
    (is (= [::subject/invalid-subject :unsupported-content-type]
           (scalar-rejection value))
        (str "Mutable legacy date/calendar must not be admitted: " (class value)))))

(deftest immutable-temporal-scalar-forms-remain-supported-test
  (let [accepted [(java.time.Instant/parse "2026-10-16T08:15:00Z")
                  (java.time.LocalDate/parse "2026-10-16")
                  (java.time.LocalDateTime/parse "2026-10-16T08:15:00")
                  (java.time.OffsetDateTime/parse "2026-10-16T08:15:00-07:00")
                  (java.time.ZonedDateTime/parse "2026-10-16T08:15:00-07:00[America/Los_Angeles]")
                  (java.util.UUID/fromString "00000000-0000-0000-0000-000000000001")]]
    (doseq [value accepted]
      (let [a (scalar-identity value)]
        (is (re-matches #"[0-9a-f]{64}" a) (str (class value)))
        (is (= a (scalar-identity value)) (str (class value)))
        (is (not= a (scalar-identity (str value)))
            (str "Temporal/UUID scalar must differ from a plain string: " (class value)))))))

(deftest nonfinite-floating-values-fail-closed-test
  (doseq [value [Double/NaN Double/POSITIVE_INFINITY Double/NEGATIVE_INFINITY
                 Float/NaN Float/POSITIVE_INFINITY Float/NEGATIVE_INFINITY]]
    (is (= [::subject/invalid-subject :unsupported-content-type]
           (scalar-rejection value))
        (str "Nonfinite floating scalar must not be admitted: " (class value)))))

(deftest nested-mutable-scalar-is-rejected-at-all-supported-container-boundaries-test
  (let [number (java.util.concurrent.atomic.AtomicLong. 41)
        date (java.util.Date. 1760595600000)
        nested [(vector :safe number)
                (list :safe date)
                {:settings {:secret number}}
                #{:safe date}
                {number :value}]]
    (doseq [value nested]
      (is (= [::subject/invalid-subject :unsupported-content-type]
             (scalar-rejection value))
          "Nested mutable values must not hide inside supported containers"))))

(deftest unsupported-scalar-errors-do-not-echo-private-inputs-test
  (let [private-object (java.util.concurrent.atomic.AtomicLong. 73829)
        private-label "private-v1036-scalar-value"
        exception (try (scalar-identity {:confidential private-label
                                         :mutable private-object})
                       nil
                       (catch clojure.lang.ExceptionInfo e e))]
    (is (instance? clojure.lang.ExceptionInfo exception))
    (is (= ::subject/invalid-subject (:error/type (ex-data exception))))
    (is (= :unsupported-content-type (:error/reason (ex-data exception))))
    (is (not (.contains (str (.getMessage exception) " " (pr-str (ex-data exception)))
                        private-label)))))

(deftest invalid-finite-state-or-unsupported-transaction-plan-rejected-test
  (is (some? (error-reason #(subject-of {[:requests request-id] nil}
                                         create-plan environment))))
  (is (some? (error-reason #(subject-of {} (assoc create-plan :entry-fn identity)
                                         environment))))
  (is (some? (error-reason #(subject-of {} (assoc create-plan :assertions
                                               [{:assert [:raw "SELECT TRUE"]}])
                                         environment)))))

(deftest omitted-required-targets-are-rejected-but-extras-unattested-test
  (is (= [::subject/invalid-subject :incomplete-subject-contract]
         (error-reason #(subject-of {} create-plan
                                    (assoc-in environment
                                              [:application :dependency-targets] [])))))
  (let [r (subject-of)
        no-writers (subject-of {} create-plan
                               (assoc-in environment [:application :writer-ids] []))]
    (is (= :bound-declarations-unattested (:status no-writers)))
    (is (false? (:application-writers-proved-closed? no-writers)))
    (is (false? (:application-dependencies-proved-complete? r)))))

(deftest exact-premise-catalog-is-required-test
  (let [original premises/derive-obligation-ledger]
    (is (= [::subject/invalid-subject :incomplete-subject-contract]
           (error-reason
            #(with-redefs [premises/derive-obligation-ledger
                           (fn [state plan]
                             (update (original state plan) :open-premises pop))]
               (subject-of)))))
    (is (= [::subject/invalid-subject :incomplete-subject-contract]
           (error-reason
            #(with-redefs [premises/derive-obligation-ledger
                           (fn [state plan]
                             (update (original state plan) :open-premises
                                     (fn [ps] (conj (subvec ps 1)
                                                    (assoc (first ps) :id :invented-premise)))))]
               (subject-of)))))
    (is (= [::subject/invalid-subject :incomplete-subject-contract]
           (error-reason
            #(with-redefs [premises/derive-obligation-ledger
                           (fn [state plan]
                             (assoc-in (original state plan)
                                       [:local-argument :status] :failed))]
               (subject-of)))))))

(deftest catalog-metadata-affects-identity-but-proves-nothing-test
  (let [a (subject-of)
        original premises/derive-obligation-ledger
        b (with-redefs [premises/derive-obligation-ledger
                        (fn [state plan]
                          (update-in (original state plan)
                                     [:open-premises 0 :needed]
                                     (constantly :changed-evidence-requirement)))]
            (subject-of))]
    (is (not= (:subject-sha256 a) (:subject-sha256 b)))
    (is (= :bound-declarations-unattested (:status b)))
    (is (= #{} (:premises-discharged b)))))

(deftest no-secret-material-or-proof-certificate-in-report-test
  (let [r (subject-of guarded-state guarded-plan environment)
        printed (pr-str r)]
    (doseq [secret [request-id membership-id organization-id
                    "private-helper-a" "private-helper-b" "private-policy"
                    digest-a]]
      (is (not (.contains printed secret))))
    (doseq [field [:abstract-state :plan :backend :application :source-sha256
                   :sql :sql-bindings :evidence :proof :physical-receipt]]
      (is (not (contains? r field))))
    (is (false? (:application-certified? r)))
    (is (= #{} (:premises-discharged r)))))

(deftest diagnostic-errors-do-not-disclose-private-subject-input-test
  (let [e (try (subject-of guarded-state guarded-plan
                           (assoc-in environment [:backend :build-sha256] "bad"))
               nil
               (catch clojure.lang.ExceptionInfo ex ex))]
    (is (instance? clojure.lang.ExceptionInfo e))
    (is (= ::subject/invalid-subject (:error/type (ex-data e))))
    (doseq [secret [request-id "private-helper-a" "private-policy" digest-a]]
      (is (not (.contains (str (.getMessage e) " " (pr-str (ex-data e))) secret))))))

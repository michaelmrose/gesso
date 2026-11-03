(ns gesso.assurance.admission-test
  "v1216: adversarial tests for the v1215 in-process structural admission
   boundary. These tests establish that caller-supplied data cannot impersonate
   a live, checker-issued handle and that consumers match their OWN expected
   subject or exact named premise. They do NOT establish a proof kernel,
   discharged premises, transportable capabilities, production authorization,
   installed-handler correspondence, or protection against hostile JVM code
   able to reflect upon and mutate private runtime state."
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing]]
            [gesso.assurance.admission :as admission]
            [gesso.assurance.assumption :as assumption]
            [gesso.assurance.subject :as subject]))

(def ^:private claim-subject
  {:kind :operation
   :id :humanhelp/claim
   :interface {:guards [:and [:equals [:field :request/status] :unclaimed]
                            [:opaque :membership/current]]
               :reads #{:request/status :request/owner :membership/status}
               :effects [[:set :request/status :claimed]
                         [:set :request/owner [:input :actor]]]
               :options {:atomic? true :version 1}}})

(def ^:private other-subject
  (assoc claim-subject :id :humanhelp/cancel))

(def ^:private base-premise
  {:id :humanhelp/claim-membership-current
   :subject claim-subject
   :scope {:kind :operation :id :humanhelp/claim}
   :proposition [:membership/authoritative-current :actor/current]
   :parameters {:actor :input/actor}
   :semantic-context {:consistency :xtdb/transaction}
   :dependencies #{:humanhelp/organization-membership}
   :origin :runtime
   :implementation-boundary :xtdb/commit-time-read})

(defn- failure
  "Return the structured failure; nil means the operation did not reject."
  [thunk]
  (try
    (thunk)
    nil
    (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn- assert-failure!
  [thunk expected-type expected-reason]
  (let [data (failure thunk)]
    (is (map? data) (str "Expected failure with reason " expected-reason))
    (is (= expected-type (:error/type data)))
    (is (= expected-reason (:reason data)))))

(defn- expect-admission-rejection!
  [thunk reason]
  (assert-failure! thunk :gesso.assurance.admission/invalid-admission reason))

(defn- expect-premise-rejection!
  [thunk reason]
  (assert-failure! thunk :gesso.assurance.assumption/invalid-assumption reason))

(defn- expect-subject-rejection!
  [thunk reason]
  (assert-failure! thunk :gesso.assurance.subject/invalid-semantic-subject reason))

(deftest subject-check-produces-only-a-structural-handle
  (let [handle (admission/admit-subject! claim-subject)
        description (admission/describe-handle handle)
        identity (subject/describe-subject claim-subject)]
    (is (admission/admitted? handle))
    (is (identical? handle (admission/require-admitted-subject!
                            handle claim-subject)))
    (is (= admission/admission-type (:type description)))
    (is (= admission/admission-version (:version description)))
    (is (= :subject-identity (:kind description)))
    (is (= :operation (:subject-kind description)))
    (is (= :humanhelp/claim (:subject-id description)))
    (is (= (:fingerprint identity) (:subject-fingerprint description)))
    (is (= :structural-only (:checking description)))
    (is (= :not-proved (:proof-status description)))
    (is (= :open (:discharge-state description)))
    (is (= :not-established (:implementation-correspondence description)))
    (is (= :none (:authority description)))
    (is (not (contains? description :certificate)))
    (is (not (contains? description :valid?)))
    (is (not (contains? description :proved?)))))

(deftest caller-constructed-handles-are-not-registered
  (let [genuine (admission/admit-subject! claim-subject)
        forged (gesso.assurance.admission.AdmissionHandle.)]
    (is (instance? gesso.assurance.admission.AdmissionHandle genuine))
    (is (instance? gesso.assurance.admission.AdmissionHandle forged))
    (is (not (identical? genuine forged)))
    (is (not (admission/admitted? forged)))
    (is (admission/admitted? genuine))
    (expect-admission-rejection!
     #(admission/describe-handle forged) :unrecognized-handle)
    (expect-admission-rejection!
     #(admission/require-admitted-subject! forged claim-subject)
     :unrecognized-handle)
    (expect-admission-rejection!
     #(admission/require-admitted-premise-match! forged base-premise base-premise)
     :unrecognized-handle)))

(deftest invalid-capability-shapes-fail-closed
  (let [genuine (admission/admit-subject! claim-subject)
        summary (admission/describe-handle genuine)]
    (doseq [value [nil false true 0 1 :verified "handle" [] #{}
                   (Object.) summary
                   {:kind :subject-identity
                    :subject-fingerprint (:subject-fingerprint summary)}]]
      (testing (str "Non-handle " (type value))
        (is (not (admission/admitted? value)))
        (expect-admission-rejection!
         #(admission/describe-handle value) :unrecognized-handle)
        (expect-admission-rejection!
         #(admission/require-admitted-subject! value claim-subject)
         :unrecognized-handle)
        (expect-admission-rejection!
         #(admission/require-admitted-premise-match!
           value base-premise base-premise)
         :unrecognized-handle)))
    (is (admission/admitted? genuine))))

(deftest maps-and-serialized-descriptions-cannot-be-replayed-as-handles
  (let [handle (admission/admit-subject! claim-subject)
        original (admission/describe-handle handle)
        copied (into {} original)
        decoded (edn/read-string (pr-str original))]
    (is (= original copied decoded))
    (is (not (identical? original decoded)))
    (doseq [candidate [original copied decoded
                       (assoc decoded :authority :proof)
                       (assoc decoded :checking :kernel-checked)
                       (assoc decoded :proof-status :proved)
                       (assoc decoded :discharge-state :discharged)
                       (assoc decoded :implementation-correspondence :established)
                       (assoc decoded :certificate {:valid? true})
                       (assoc decoded :valid? true)
                       (assoc decoded :type admission/admission-type)]]
      (is (not (admission/admitted? candidate)))
      (expect-admission-rejection!
       #(admission/require-admitted-subject! candidate claim-subject)
       :unrecognized-handle))
    (is (identical? handle (admission/require-admitted-subject!
                            handle claim-subject)))))

(deftest descriptions-are-data-not-authenticated-claims
  (let [handle (admission/admit-subject! claim-subject)
        a (admission/describe-handle handle)
        b (admission/describe-handle handle)]
    (is (= a b))
    (is (= #{:kind :subject-fingerprint :subject-kind :subject-id
             :type :version :checking :proof-status :discharge-state
             :implementation-correspondence :authority}
           (set (keys a))))
    (is (not (identical? handle a)))
    (is (not (admission/admitted? a)))
    (is (not (admission/admitted? b)))
    (is (= :none (:authority a)))
    (is (= :not-proved (:proof-status b)))))

(deftest exact-subject-binding-rejects-every-semantic-change
  (let [handle (admission/admit-subject! claim-subject)
        changed-subjects
        [(assoc claim-subject :id :humanhelp/other-claim)
         (assoc claim-subject :kind :rule)
         (assoc claim-subject :interface {:guards [:and]})
         (assoc-in claim-subject [:interface :options :version] 2)
         (assoc-in claim-subject [:interface :options :atomic?] false)
         (assoc-in claim-subject [:interface :guards 1] [:opaque :membership/other])
         (assoc-in claim-subject [:interface :effects 1] [:set :request/owner :other])
         (update-in claim-subject [:interface :reads] conj :request/secret)
         (assoc claim-subject :id (:id other-subject))]]
    (doseq [changed changed-subjects]
      (testing (str "Mutated subject " (pr-str changed))
        (is (not= (:fingerprint (subject/describe-subject claim-subject))
                  (:fingerprint (subject/describe-subject changed))))
        (expect-admission-rejection!
         #(admission/require-admitted-subject! handle changed)
         :wrong-subject)))
    (is (admission/admitted? handle))
    (is (identical? handle (admission/require-admitted-subject!
                            handle claim-subject)))))

(deftest structurally-equivalent-source-forms-can-share-an-identity
  (let [same-subject {:interface {:options {:version 1N :atomic? true}
                                  :effects [[:set :request/status :claimed]
                                            [:set :request/owner [:input :actor]]]
                                  :reads #{:membership/status :request/owner :request/status}
                                  :guards [:and
                                           [:equals [:field :request/status] :unclaimed]
                                           [:opaque :membership/current]]}
                      :id :humanhelp/claim :kind :operation}
        handle (admission/admit-subject! claim-subject)
        duplicate (admission/admit-subject! same-subject)]
    (is (= (:fingerprint (subject/describe-subject claim-subject))
           (:fingerprint (subject/describe-subject same-subject))))
    (is (identical? handle (admission/require-admitted-subject!
                            handle same-subject)))
    (is (identical? duplicate (admission/require-admitted-subject!
                               duplicate claim-subject)))
    (is (not (identical? handle duplicate)))
    (is (admission/admitted? handle))
    (is (admission/admitted? duplicate))
    ;; Issuance is not proof authority, and is not uniqueness across subjects.
    (is (= :none (:authority (admission/describe-handle duplicate))))))

(deftest a-matching-subject-name-is-not-enough
  (let [handle (admission/admit-subject! claim-subject)]
    (doseq [changed [(assoc-in claim-subject [:interface :options :version] 999)
                     (assoc-in claim-subject [:interface :guards 1]
                               [:opaque :membership/always-true])]]
      (is (= (:id claim-subject) (:id changed)))
      (expect-admission-rejection!
       #(admission/require-admitted-subject! handle changed)
       :wrong-subject))))

(deftest forged-success-fields-on-the-original-subject-fail-validation
  (doseq [[field value] [[:fingerprint "sha256:fake"]
                         [:valid? true]
                         [:proved? true]
                         [:admission :admitted]
                         [:certificate {:result :proved}]
                         [:authority :proof]
                         [:checking :kernel-checked]]]
    (testing (str "Injected subject field " field)
      (expect-subject-rejection!
       #(admission/admit-subject! (assoc claim-subject field value))
       :wrong-keys))))

(deftest arbitrary-executable-code-cannot-become-declarative-interface
  (let [executed? (atom false)
        malicious (fn [] (reset! executed? true) :verified)]
    (expect-subject-rejection!
     #(admission/admit-subject!
       (assoc-in claim-subject [:interface :executor] malicious))
     :unsupported-interface-value)
    (expect-subject-rejection!
     #(admission/admit-subject!
       (assoc-in claim-subject [:interface :runtime] (atom {:valid? true})))
     :unsupported-interface-value)
    (is (false? @executed?))))

(deftest premise-match-handle-is-open-and-non-authoritative
  (let [handle (admission/admit-premise-match! base-premise base-premise)
        description (admission/describe-handle handle)
        required (assumption/describe-assumption base-premise)
        subject-id (subject/describe-subject claim-subject)]
    (is (admission/admitted? handle))
    (is (identical? handle (admission/require-admitted-premise-match!
                            handle base-premise base-premise)))
    (is (= :premise-structural-match (:kind description)))
    (is (= (:fingerprint subject-id) (:subject-fingerprint description)))
    (is (= (:premise-fingerprint required)
           (:premise-fingerprint description)))
    (is (= :structural-only (:checking description)))
    (is (= :not-proved (:proof-status description)))
    (is (= :open (:discharge-state description)))
    (is (= :none (:authority description)))
    (is (= :not-established (:implementation-correspondence description)))))

(deftest a-premise-can-match-itself-without-discharging-it
  (let [handle (admission/admit-premise-match! base-premise base-premise)
        desc (admission/describe-handle handle)]
    (is (= :open (:discharge-state desc)))
    (is (= :not-proved (:proof-status desc)))
    (is (= :none (:authority desc)))
    (is (= :structural-only (:checking desc)))
    (is (= :not-admitted (:admission (assumption/describe-assumption base-premise))))))

(deftest admission-rejects-mismatched-required-and-offered-premises
  (doseq [[mutant expected]
          [[(assoc base-premise :scope {:kind :operation :id :humanhelp/cancel})
            :scope-mismatch]
           [(assoc base-premise :subject other-subject) :subject-mismatch]
           [(assoc base-premise :proposition [:membership/administrator])
            :premise-mismatch]
           [(assoc base-premise :id :humanhelp/claim-any-member)
            :premise-mismatch]
           [(assoc base-premise :parameters {:actor :input/other})
            :premise-mismatch]
           [(assoc base-premise :semantic-context {:consistency :eventual})
            :premise-mismatch]
           [(assoc base-premise :dependencies #{})
            :premise-mismatch]
           [(assoc base-premise :origin :external-service)
            :premise-mismatch]
           [(assoc base-premise :implementation-boundary :xtdb/preflight)
            :premise-mismatch]]]
    (testing (str "Cannot issue match for mutant " (pr-str mutant))
      (expect-premise-rejection!
       #(admission/admit-premise-match! base-premise mutant)
       expected))))

(deftest premise-handle-cannot-replay-under-a-different-premise
  (let [handle (admission/admit-premise-match! base-premise base-premise)
        mutations [(assoc base-premise :proposition [:membership/administrator])
                   (assoc base-premise :parameters {:actor :input/other})
                   (assoc base-premise :dependencies #{})
                   (assoc base-premise :origin :external-service)
                   (assoc base-premise :implementation-boundary :xtdb/preflight)
                   (assoc base-premise :scope {:kind :operation :id :humanhelp/other})
                   (assoc base-premise :subject other-subject)]]
    (doseq [mutant mutations]
      (testing (str "Premise mutation " (pr-str mutant))
        ;; Two identical *mutated* declarations match structurally, but the
        ;; pre-existing handle is scoped to the original premise identity.
        (is (= :structural-only
               (:match (assumption/match-premise! mutant mutant))))
        (expect-admission-rejection!
         #(admission/require-admitted-premise-match! handle mutant mutant)
         :wrong-premise)))
    (is (identical? handle (admission/require-admitted-premise-match!
                            handle base-premise base-premise)))))

(deftest premise-consumer-rechecks-both-raw-declarations
  (let [handle (admission/admit-premise-match! base-premise base-premise)
        mutant (assoc base-premise :proposition [:membership/admin])]
    (expect-premise-rejection!
     #(admission/require-admitted-premise-match! handle base-premise mutant)
     :premise-mismatch)
    (expect-premise-rejection!
     #(admission/require-admitted-premise-match! handle mutant base-premise)
     :premise-mismatch)
    (expect-premise-rejection!
     #(admission/require-admitted-premise-match!
       handle base-premise (assoc base-premise :subject other-subject))
     :subject-mismatch)
    (expect-premise-rejection!
     #(admission/require-admitted-premise-match!
       handle base-premise (assoc base-premise :scope {:kind :operation :id :humanhelp/other}))
     :scope-mismatch)))

(deftest incompatible-structural-handle-kinds-are-not-interchangeable
  (let [subject-handle (admission/admit-subject! claim-subject)
        premise-handle (admission/admit-premise-match! base-premise base-premise)]
    (expect-admission-rejection!
     #(admission/require-admitted-subject! premise-handle claim-subject)
     :wrong-check-kind)
    (expect-admission-rejection!
     #(admission/require-admitted-premise-match!
       subject-handle base-premise base-premise)
     :wrong-check-kind)
    (is (admission/admitted? subject-handle))
    (is (admission/admitted? premise-handle))))

(deftest altered-premise-descriptions-cannot-serve-as-evidence
  (let [handle (admission/admit-premise-match! base-premise base-premise)
        description (admission/describe-handle handle)]
    (doseq [copy [(into {} description)
                  (edn/read-string (pr-str description))
                  (assoc description :discharge-state :discharged)
                  (assoc description :proof-status :proved)
                  (assoc description :authority :kernel-verified)
                  (assoc description :valid? true)]]
      (is (not (admission/admitted? copy)))
      (expect-admission-rejection!
       #(admission/require-admitted-premise-match!
         copy base-premise base-premise)
       :unrecognized-handle))
    (is (= :open (:discharge-state (admission/describe-handle handle))))))

(deftest alleged-discharge-fields-on-raw-premises-cannot-be-admitted
  (doseq [[field value]
          [[:discharge-state :discharged]
           [:proof-status :proved]
           [:valid? true]
           [:certificate {:checking :kernel-checked}]
           [:evidence {:class :kernel-checked}]
           [:admission :admitted]]]
    (testing (str "Injected premise field " field)
      (expect-premise-rejection!
       #(admission/admit-premise-match!
         (assoc base-premise field value)
         (assoc base-premise field value))
       :wrong-keys))))

(deftest raw-descriptions-and-purported-proof-receipts-are-not-premises
  (let [description (assumption/describe-assumption base-premise)
        copied (edn/read-string (pr-str description))]
    (doseq [candidate [description copied
                       {:premise-fingerprint (:premise-fingerprint description)
                        :subject-fingerprint (:subject-fingerprint description)
                        :discharge-state :discharged
                        :checking :kernel-checked}]]
      (expect-premise-rejection!
       #(admission/admit-premise-match! base-premise candidate)
       :wrong-keys))))

(deftest repeated-admission-does-not-upgrade-proof-authority
  (let [subject-handles (repeatedly 8 #(admission/admit-subject! claim-subject))
        premise-handles (repeatedly 8 #(admission/admit-premise-match!
                                        base-premise base-premise))]
    (is (= 8 (count (distinct subject-handles))))
    (is (= 8 (count (distinct premise-handles))))
    (doseq [handle (concat subject-handles premise-handles)]
      (is (admission/admitted? handle))
      (let [description (admission/describe-handle handle)]
        (is (= :structural-only (:checking description)))
        (is (= :not-proved (:proof-status description)))
        (is (= :open (:discharge-state description)))
        (is (= :none (:authority description)))))))

(deftest interleaved-admissions-preserve-kind-and-subject-binding
  (let [subject-handles (vec (repeatedly 6 #(admission/admit-subject! claim-subject)))
        other-handles (vec (repeatedly 6 #(admission/admit-subject! other-subject)))
        premise-handles (vec (repeatedly 6 #(admission/admit-premise-match!
                                             base-premise base-premise)))]
    (doseq [handle subject-handles]
      (is (identical? handle (admission/require-admitted-subject!
                              handle claim-subject)))
      (expect-admission-rejection!
       #(admission/require-admitted-subject! handle other-subject)
       :wrong-subject))
    (doseq [handle other-handles]
      (is (identical? handle (admission/require-admitted-subject!
                              handle other-subject)))
      (expect-admission-rejection!
       #(admission/require-admitted-subject! handle claim-subject)
       :wrong-subject))
    (doseq [handle premise-handles]
      (is (identical? handle (admission/require-admitted-premise-match!
                              handle base-premise base-premise)))
      (expect-admission-rejection!
       #(admission/require-admitted-subject! handle claim-subject)
       :wrong-check-kind))))

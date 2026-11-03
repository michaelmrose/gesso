(ns gesso.assurance.assumption-test
  "v1214: adversarial regressions for named, subject-bound, *unadmitted*
   assumptions introduced in v1213.

   These tests establish structural identity, strict admission INPUT shape,
   and the fact that matching a premise does not discharge it. They do NOT
   prove the proposition, authenticate alleged evidence, check actual XTDB
   behavior, or establish implementation correspondence. Those obligations
   remain with later Gesso assurance layers."
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing]]
            [gesso.assurance.assumption :as assumption]
            [gesso.assurance.claim :as claim]
            [gesso.assurance.subject :as subject]))

(defrecord ParamsRecord [actor])

(def ^:private base
  {:id :humanhelp/current-membership
   :subject {:kind :operation
             :id :humanhelp/claim
             :interface {:reads #{:request/status :membership/status}
                         :guards [[:= [:field :request/status] :unclaimed]]
                         :effects [[:set :request/owner [:input :actor]]]}}
   :scope {:kind :operation :id :humanhelp/claim}
   :proposition [:membership/authoritative-current]
   :parameters {:actor :humanhelp/user-a}
   :semantic-context {:basis 4 :realization :xtdb}
   :dependencies #{:gesso/atomic-current-read}
   :origin :runtime
   :implementation-boundary :xtdb/transaction})

(defn- describe [value]
  (assumption/describe-assumption value))

(defn- premise-id [value]
  (:premise-fingerprint (describe value)))

(defn- error-data [f]
  (try
    (f)
    nil
    (catch clojure.lang.ExceptionInfo e
      (ex-data e))))

(defn- invalid [value]
  (error-data #(describe value)))

(defn- invalid-match [required offered]
  (error-data #(assumption/match-premise! required offered)))

(defn- expect-invalid!
  [value reason at]
  (let [failure (invalid value)]
    (is (map? failure) (str "Expected invalid assumption: " (pr-str value)))
    (is (= :gesso.assurance.assumption/invalid-assumption (:error/type failure)))
    (is (= reason (:reason failure)))
    (when at (is (= at (:at failure))))))

(defn- expect-different!
  [value]
  (let [original (describe base)
        changed (describe value)]
    (is (not= (:premise-fingerprint original)
              (:premise-fingerprint changed))
        (str "Mutant must change premise identity: " (pr-str value)))
    (is (= :open (:discharge-state changed)))
    (is (= :not-admitted (:admission changed)))))

(deftest description-is-closed-and-unadmitted
  (let [result (describe base)]
    (is (= #{:type :version :assumption :subject-fingerprint
             :premise-fingerprint :discharge-state :checking :admission}
           (set (keys result))))
    (is (= assumption/description-type (:type result)))
    (is (= :gesso.assurance.assumption/unadmitted-description (:type result)))
    (is (= assumption/description-version (:version result)))
    (is (= 1 (:version result)))
    (is (= base (:assumption result)))
    (is (= :open (:discharge-state result)))
    (is (= :not-performed (:checking result)))
    (is (= :not-admitted (:admission result)))
    (is (re-matches #"sha256:[0-9a-f]{64}" (:subject-fingerprint result)))
    (is (re-matches #"sha256:[0-9a-f]{64}" (:premise-fingerprint result)))
    (is (not= (:subject-fingerprint result) (:premise-fingerprint result)))
    (doseq [forbidden [:proof :certificate :valid? :verified? :checked?
                       :discharged? :discharge :trusted? :authority]]
      (is (not (contains? result forbidden))))))

(deftest discharge-vocabulary-does-not-confer-discharge
  (is (= #{:open :discharged} assumption/discharge-states))
  (is (not (contains? assumption/discharge-states :trusted)))
  (is (not (contains? assumption/discharge-states :runtime-enforced)))
  (is (not (contains? assumption/discharge-states :kernel-checked)))
  (doseq [origin assumption/assumption-origins]
    (let [result (describe (assoc base :origin origin))]
      (is (= origin (get-in result [:assumption :origin])))
      (is (= :open (:discharge-state result)))
      (is (= :not-performed (:checking result)))
      (is (= :not-admitted (:admission result)))))
  (is (= #{:opaque-clojure :external-service :application-policy
           :environment :runtime :implementation-correspondence
           :distributed-protocol :other}
         assumption/assumption-origins)))

(deftest missing-top-level-fields-fail-closed
  (doseq [field (keys base)]
    (let [failure (invalid (dissoc base field))]
      (testing (str "Required field " field)
        (is (= :gesso.assurance.assumption/invalid-assumption
               (:error/type failure)))
        (is (= :wrong-keys (:reason failure)))
        (is (= :assumption (:at failure)))
        (is (= #{field} (:missing failure)))
        (is (= #{} (:unexpected failure)))))))

(deftest fabricated-discharge-and-authority-fields-fail-closed
  (doseq [[field value] [[:discharge-state :discharged]
                         [:discharge :discharged]
                         [:discharged? true]
                         [:admission :admitted]
                         [:checking :kernel-checked]
                         [:valid? true]
                         [:verified? true]
                         [:trusted? true]
                         [:authority :gesso/accept]
                         [:proof {:checked true}]
                         [:certificate {:valid? true}]
                         [:type assumption/description-type]
                         [:version 1]
                         [:premise-fingerprint (premise-id base)]
                         [:subject-fingerprint (:subject-fingerprint
                                                (describe base))]]]
    (testing (str "Caller-supplied " field)
      (let [failure (invalid (assoc base field value))]
        (is (= :gesso.assurance.assumption/invalid-assumption
               (:error/type failure)))
        (is (= :wrong-keys (:reason failure)))
        (is (= #{field} (:unexpected failure)))
        (is (= #{} (:missing failure)))))))

(deftest malformed-top-level-values-fail-closed
  (doseq [bad [nil false :humanhelp/current-membership
               [] [:membership/authoritative-current] #{} "assumption" 7]]
    (expect-invalid! bad :expected-map :assumption))
  (doseq [bad [:missing :unqualified nil 4 "humanhelp/membership" [] {}]]
    (expect-invalid! (assoc base :id bad) :invalid-id :id)))

(deftest scope-is-a-closed-qualified-identity
  (doseq [bad [nil [] :operation "operation"
               {:kind :operation}
               {:id :humanhelp/claim}
               {:kind :unknown :id :humanhelp/claim}
               {:kind :operation :id :unqualified}
               {:kind :operation :id nil}
               {:kind :operation :id :humanhelp/claim :discharged? true}]]
    (let [failure (invalid (assoc base :scope bad))]
      (is (= :gesso.assurance.assumption/invalid-assumption
             (:error/type failure)))
      (is (contains? #{:expected-map :wrong-keys :unknown-scope-kind
                       :invalid-scope-id} (:reason failure))))))

(deftest proposition-is-symbolic-not-a-proved-formula
  (doseq [bad [nil false :membership/authoritative-current []
               [:unqualified] [nil] ["membership/authoritative-current"]
               '(:membership/authoritative-current) {}]]
    (expect-invalid! (assoc base :proposition bad)
                     :invalid-proposition :proposition))
  ;; A qualified operator is still just symbolic data. It is deliberately not
  ;; checked for semantics or for validity of its arguments at this stage.
  (let [result (describe (assoc base :proposition
                                [:application/unknown-opaque-condition :symbolic/arg]))]
    (is (= :open (:discharge-state result)))
    (is (= :not-performed (:checking result)))
    (is (= :not-admitted (:admission result)))))

(deftest parameters-and-context-must-be-declarative-maps
  (doseq [field [:parameters :semantic-context]
          bad [nil false [] #{} :invalid 123 "map" (fn [] true)
               (->ParamsRecord :humanhelp/user-a)]]
    (expect-invalid! (assoc base field bad) :expected-map field)))

(deftest dependencies-require-named-keywords
  (doseq [bad [nil [] {} :gesso/atomic-current-read
               #{:unqualified} #{nil} #{1} #{"gesso/atomic-current-read"}
               [:gesso/atomic-current-read]]]
    (expect-invalid! (assoc base :dependencies bad)
                     :invalid-dependencies :dependencies))
  (is (= :not-admitted
         (:admission (describe (assoc base :dependencies #{})))))
  (is (= :not-admitted
         (:admission (describe (assoc base :dependencies
                                       #{:gesso/read :gesso/write}))))))

(deftest origins-and-boundaries-remain-declarative
  (doseq [bad [nil :unqualified :not-a-source "runtime" false 3]]
    (expect-invalid! (assoc base :origin bad) :unknown-origin :origin))
  (doseq [bad [:unqualified "xtdb/transaction" true 7 [] {}]]
    (expect-invalid! (assoc base :implementation-boundary bad)
                     :invalid-implementation-boundary :implementation-boundary))
  (let [result (describe (assoc base :implementation-boundary nil))]
    (is (= :open (:discharge-state result)))
    (is (= :not-admitted (:admission result)))))

(deftest subject-identity-is-required-and-recomputed
  (doseq [bad [nil [] false :humanhelp/claim
               {:kind :operation :id :humanhelp/claim}
               {:kind :operation :id :humanhelp/claim :interface {}}
               {:kind :operation :id :humanhelp/claim :interface {}
                :fingerprint "sha256:pretend"}]]
    (let [failure (invalid (assoc base :subject bad))]
      (is (map? failure))
      (is (contains? #{:gesso.assurance.assumption/invalid-assumption
                       :gesso.assurance.subject/invalid-semantic-subject}
                     (:error/type failure)))))
  (let [subject-id (:fingerprint (subject/describe-subject (:subject base)))]
    (is (= subject-id (:subject-fingerprint (describe base))))
    (is (= subject-id (:subject-fingerprint
                       (describe (assoc base :proposition [:membership/different])))))))

(deftest opaque-functions-cannot-enter-canonical-subject-or-premise
  (doseq [mutant [(assoc-in base [:subject :interface :opaque] (fn [] :approved))
                  (assoc-in base [:parameters :callback] (fn [] :approved))
                  (assoc-in base [:semantic-context :external] (Object.))
                  (assoc base :proposition [:membership/test (fn [] true)])]]
    (let [failure (invalid mutant)]
      (is (= :gesso.assurance.subject/invalid-semantic-subject
             (:error/type failure)))
      (is (= :unsupported-interface-value (:reason failure))))))

(deftest identities-are-invariant-under-map-set-order-and-source-metadata
  (let [reordered (-> base
                      (update :subject
                              (fn [s] (into (array-map) (reverse (seq s)))))
                      (update-in [:subject :interface :reads]
                                 #(into (sorted-set) %))
                      (update :parameters
                              (fn [m] (into (sorted-map) m)))
                      (update :dependencies
                              #(into (sorted-set) %))
                      (with-meta {:file "different-file.clj" :line 300}))
        normalized (-> base
                       (assoc-in [:semantic-context :basis] 4N))
        two-dependencies (assoc base :dependencies #{:gesso/read :gesso/write})
        reordered-dependencies (assoc base :dependencies
                                      (into (sorted-set) [:gesso/write :gesso/read]))]
    (is (= (premise-id base) (premise-id reordered)))
    (is (= (premise-id base) (premise-id normalized)))
    (is (= (:subject-fingerprint (describe base))
           (:subject-fingerprint (describe reordered))))
    (is (= (:subject-fingerprint (describe base))
           (:subject-fingerprint (describe normalized))))
    (is (= (premise-id two-dependencies)
           (premise-id reordered-dependencies)))))

(deftest all-premise-fields-are-part-of-identity
  (doseq [mutant [(assoc base :id :humanhelp/expired-membership)
                 (assoc-in base [:subject :id] :humanhelp/unclaim)
                 (assoc-in base [:subject :interface :reads] #{:request/status})
                 (assoc-in base [:subject :interface :effects 0 2] :other)
                 (assoc-in base [:scope :id] :humanhelp/unclaim)
                 (assoc-in base [:scope :kind] :application)
                 (assoc base :proposition [:membership/stale-or-incorrect])
                 (assoc base :proposition [:membership/authoritative-current 4])
                 (assoc-in base [:parameters :actor] :humanhelp/user-b)
                 (assoc-in base [:parameters :unused] false)
                 (assoc base :parameters {})
                 (assoc-in base [:semantic-context :basis] 5)
                 (assoc-in base [:semantic-context :realization] :in-memory)
                 (assoc-in base [:semantic-context :extra] nil)
                 (assoc base :semantic-context {})
                 (assoc base :dependencies #{})
                 (assoc base :dependencies #{:gesso/atomic-current-read
                                              :gesso/transaction-isolation})
                 (assoc base :origin :external-service)
                 (assoc base :implementation-boundary :xtdb/snapshot)
                 (assoc base :implementation-boundary nil)]]
    (testing (pr-str mutant)
      (expect-different! mutant))))

(deftest same-name-does-not-merge-different-assumptions
  (doseq [mutant [(assoc base :proposition [:membership/old-read])
                 (assoc-in base [:subject :interface :guards 0 2] :claimed)
                 (assoc-in base [:scope :id] :humanhelp/unclaim)
                 (assoc-in base [:parameters :actor] :humanhelp/user-b)
                 (assoc-in base [:semantic-context :basis] 7)
                 (assoc base :implementation-boundary :xtdb/preflight-only)]]
    (is (= (:id base) (:id mutant)))
    (is (not= (premise-id base) (premise-id mutant)))))

(deftest match-only-reports-structural-agreement-not-admission
  (let [matched (assumption/match-premise! base base)]
    (is (= #{:match :required-premise :offered-premise :discharge-state
             :checking :admission} (set (keys matched))))
    (is (= :structural-only (:match matched)))
    (is (= (premise-id base) (:required-premise matched)))
    (is (= (premise-id base) (:offered-premise matched)))
    (is (= :open (:discharge-state matched)))
    (is (= :not-performed (:checking matched)))
    (is (= :not-admitted (:admission matched)))
    (is (not (contains? matched :proof)))
    (is (not (contains? matched :certificate)))))

(deftest wrong-scope-fails-before-other-premise-comparisons
  (doseq [scope [{:kind :operation :id :humanhelp/unclaim}
                 {:kind :application :id :humanhelp/claim}]]
    (let [mutant (assoc base :scope scope)
          failure (invalid-match base mutant)]
      (is (= :gesso.assurance.assumption/invalid-assumption
             (:error/type failure)))
      (is (= :scope-mismatch (:reason failure)))
      (is (= (:scope base) (:required failure)))
      (is (= scope (:offered failure)))))
  (let [mutant (-> base
                   (assoc :scope {:kind :operation :id :humanhelp/unclaim})
                   (assoc :proposition [:membership/forged]))]
    (is (= :scope-mismatch (:reason (invalid-match base mutant))))))

(deftest wrong-subject-fails-even-when-name-proposition-and-scope-match
  (doseq [subject-map [(assoc-in (:subject base) [:interface :guards 0 2] :claimed)
                       (assoc-in (:subject base) [:interface :reads] #{:request/status})
                       (assoc (:subject base) :id :humanhelp/unclaim)]]
    (let [mutant (assoc base :subject subject-map)
          failure (invalid-match base mutant)]
      (is (= (:id base) (:id mutant)))
      (is (= (:scope base) (:scope mutant)))
      (is (= (:proposition base) (:proposition mutant)))
      (is (= :subject-mismatch (:reason failure)))
      (is (= (:subject-fingerprint (describe base)) (:required failure)))
      (is (= (:subject-fingerprint (describe mutant)) (:offered failure))))))

(deftest wrong-proposition-context-dependencies-and-boundary-fail
  (doseq [mutant [(assoc base :id :humanhelp/another-name)
                 (assoc base :proposition [:membership/wrong-proposition])
                 (assoc base :parameters {:actor :humanhelp/other-user})
                 (assoc base :semantic-context {:basis 5 :realization :xtdb})
                 (assoc base :dependencies #{})
                 (assoc base :origin :environment)
                 (assoc base :implementation-boundary :xtdb/preflight)]]
    (let [failure (invalid-match base mutant)]
      (is (= :premise-mismatch (:reason failure)) (pr-str mutant))
      (is (= :gesso.assurance.assumption/invalid-assumption
             (:error/type failure)))
      (is (= (premise-id base) (:required failure)))
      (is (= (premise-id mutant) (:offered failure)))
      (is (= :premise-mismatch
             (:reason (invalid-match mutant base)))))))

(deftest purported-discharge-is-not-a-raw-assumption
  (let [description (describe base)
        altered (assoc description :discharge-state :discharged
                       :checking :kernel-checked :admission :admitted)]
    (doseq [offered [description altered (assoc description :valid? true)]]
      (let [failure (invalid-match base offered)]
        (is (= :wrong-keys (:reason failure)))
        (is (= :assumption (:at failure)))
        (is (= :gesso.assurance.assumption/invalid-assumption
               (:error/type failure))))))
  (doseq [forged [(assoc base :discharge-state :discharged)
                  (assoc base :certificate {:subject (premise-id base)
                                            :admission :admitted})
                  (assoc base :evidence {:class :kernel-checked
                                         :reference :proof/copied})]]
    (is (= :wrong-keys (:reason (invalid-match base forged))))))

(deftest copied-declarations-do-not-constitute-discharge
  (let [copied (edn/read-string (pr-str base))
        matched (assumption/match-premise! base copied)]
    (is (= base copied))
    (is (= :structural-only (:match matched)))
    (is (= :open (:discharge-state matched)))
    (is (= :not-performed (:checking matched)))
    (is (= :not-admitted (:admission matched)))))

(deftest claimed-kernel-evidence-cannot-discharge-an-assumption
  (let [claim-description
        (claim/describe-claim
         {:subject :humanhelp/claim
          :proposition [:membership/authoritative-current]
          :scope {:kind :operation :id :humanhelp/claim}
          :evidence {:class :kernel-checked :reference :proof/unverified}
          :assumptions #{:humanhelp/current-membership}
          :open-obligations #{}
          :dependencies #{}
          :implementation-boundary :xtdb/transaction})]
    (is (= :not-admitted (:admission claim-description)))
    (is (= :not-performed (:checking claim-description)))
    (is (true? (:conditional? claim-description)))
    (is (= :open (:discharge-state (describe base))))
    (is (= :wrong-keys
           (:reason (invalid-match base claim-description))))))

(deftest premise-matching-is-not-temporal-or-runtime-attestation
  ;; The raw contract is a *descriptor*. Matching two copies cannot attest
  ;; current authentication, current database basis, a live external API, or
  ;; correct behavior at the declared implementation boundary.
  (doseq [origin [:runtime :environment :external-service
                  :implementation-correspondence :opaque-clojure]]
    (let [premise (assoc base :origin origin)
          match (assumption/match-premise! premise premise)]
      (is (= :structural-only (:match match)))
      (is (= :open (:discharge-state match)))
      (is (= :not-admitted (:admission match))))))

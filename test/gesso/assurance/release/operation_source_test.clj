(ns gesso.assurance.release.operation-source-test
  "v2264: Permanent selected-source release admission and adversarial checks.

   Exercise the REAL operation-set checker against a nonempty singleton and
   a multi-declaration Claim catalog. Independently observe every generated
   omission (including those rejected by the original obligation checker),
   and test that unexpected errors, unchanged fingerprints, source drift, and
   swapped checkers cannot turn into green candidate evidence. No instance,
   deployment, runtime permission, or release credential is established here."
  (:require [clojure.set :as set]
            [clojure.test :refer [deftest is testing]]
            [gesso.assurance.proof.operation-set :as operation-set]
            [gesso.assurance.proof.operation-set-test] ; original validated catalog
            [gesso.assurance.release.operation-source :as source]))

(def ^:private original-check operation-set/check-operation-set!)
(def ^:private original-test-catalog
  (var-get (or (ns-resolve 'gesso.assurance.proof.operation-set-test 'catalog)
               (throw (ex-info "Original operation-set fixture absent" {})))))

(defn- catalog [] original-test-catalog)

(defn- single-catalog []
  (-> (catalog)
      (assoc :operations (vec (take 1 (:operations (catalog))))
             :declared-operation-ids #{:request/claim}
             :invariants (select-keys (:invariants (catalog)) [:request/truth]))))

(defn- evaluate [c]
  (source/assess-original-catalog! (constantly c)))

(defn- candidate-rows [report]
  (get-in report [:selected-release-diagnostic :decisions :candidate :rows]))

(defn- report-status [r phase]
  (get-in r [:selected-release-diagnostic :decisions phase :status]))

(defn- assert-no-credential! [r]
  (doseq [[k expected] {:release-authority :none
                        :runtime-effect-permission :none
                        :candidate-admission :not-issued
                        :installation-eligibility :not-issued
                        :activation-eligibility :not-issued
                        :completion-claim :not-issued}]
    (is (= expected (get r k)) (str "No credential: " k)))
  (is (not= :selected-checks-passed-only (:status r)))
  (is (= :blocked (:status r)))
  (is (= :not-examined (:activation-status r)))
  (is (= :not-examined (:completion-status r))))

(defn- assert-candidate-only! [c]
  (let [checked (original-check c)
        r (evaluate c)
        rows (candidate-rows r)
        required (+ 3 (count (:operations c)) (count (:invariants c)))
        diag (:selected-release-diagnostic r)]
    (is (= source/report-type (:type r)))
    (is (= source/report-version (:version r)))
    (is (= (:catalog-id checked) (:selected-catalog-id r)))
    (is (= (:fingerprint checked) (:selected-catalog-fingerprint r)))
    (is (= (:declared-operation-ids checked) (:operation-ids r)))
    (is (= (:invariant-ids checked) (:invariant-ids r)))
    (is (= :host-supplied-original-catalog-not-installed-authenticated
           (:source-selection r)))
    (is (= :checks-passed-only (:candidate-status r)))
    (is (= :checks-passed-only (report-status r :candidate)))
    (is (= :blocked (:installation-status r)))
    (is (= :blocked (report-status r :installation)))
    (is (= :not-examined (report-status r :activation)))
    (is (= :not-examined (report-status r :completion)))
    (is (= required (count rows)))
    (is (= #{:passed} (set (map :status (vals rows)))))
    (is (= :matched (get-in diag [:initial-inventory :status])))
    (is (= :matched (get-in diag [:final-inventory :status])))
    (is (= :passed (get-in rows [:gesso.release.source/omitted-operation :status])))
    (is (= :passed (get-in rows [:gesso.release.source/omitted-invariant :status])))
    (is (= :negative (get-in rows [:gesso.release.source/omitted-operation :kind])))
    (is (= :negative (get-in rows [:gesso.release.source/omitted-invariant :kind])))
    (assert-no-credential! r)
    r))

;; A controlled checker wrapper is installed at BOTH pinned Vars, so that
;; operation-source's replacement check remains satisfied. The wrapper delegates
;; to the unmodified real checker except in a specifically adversarial test.
;; We do not use these wrappers to claim external or original-source authority.
(defn- with-checker [f run!]
  (let [captured (ns-resolve 'gesso.assurance.release.operation-source
                             'original-operation-check)]
    (when-not (var? captured)
      (throw (ex-info "Original checker capture missing" {})))
    (with-redefs-fn {#'operation-set/check-operation-set! f
                     captured f}
      run!)))

(defn- ids [c]
  {:operations (set (map :id (:operations c)))
   :invariants (set (keys (:invariants c)))})

(defn- omitted [original seen]
  {:operations (set/difference (:operations (ids original))
                                       (:operations (ids seen)))
   :invariants (set/difference (:invariants (ids original))
                                        (:invariants (ids seen)))})

(defn- source-ex-info [f]
  (try
    (f)
    nil
    (catch clojure.lang.ExceptionInfo e (ex-data e))))

(deftest one-operation-one-invariant-qualifies-candidate-only
  (testing "A singleton invariant's lower-layer structural rejection counts as detection"
    (let [c (single-catalog)
          r (assert-candidate-only! c)]
      (is (= #{:request/claim} (:operation-ids r)))
      (is (= #{:request/truth} (:invariant-ids r)))
      (is (true? (:conditional? r)))
      (is (= #{:gesso.release.premise/source-obligations-open}
             (get-in r [:selected-release-diagnostic :accepted-premises]))))))

(deftest all-three-operations-and-four-invariants-qualify-candidate-only
  (let [r (assert-candidate-only! (catalog))]
    (is (= #{:request/claim :request/unclaim :request/complete}
           (:operation-ids r)))
    (is (= 4 (count (:invariant-ids r))))
    (is (true? (:conditional? r)))))

(deftest source-omission-controls-visit-every-declaration
  (doseq [c [(single-catalog) (catalog)]]
    (let [seen (atom [])
          recording (fn [candidate]
                      (swap! seen conj (omitted c candidate))
                      (original-check candidate))
          r (with-checker recording #(evaluate c))
          observed (set @seen)
          expected (into #{}
                         (concat (for [id (:operations (ids c))]
                                   {:operations #{id} :invariants #{}})
                                 (for [id (:invariants (ids c))]
                                   {:operations #{} :invariants #{id}})))]
      (is (= :checks-passed-only (:candidate-status r)))
      (is (set/subset? expected observed)
          (str "Every original entry must be independently removed: "
               (pr-str (set/difference expected observed))))
      (is (contains? observed {:operations #{} :invariants #{}}))
      (is (not-any? #(or (> (count (:operations %)) 1)
                         (> (count (:invariants %)) 1)
                         (and (seq (:operations %)) (seq (:invariants %))))
                    observed))
      (assert-no-credential! r))))

(deftest unrelated-checker-failures-cannot-become-mutation-successes
  (let [c (catalog)
        target :request/version-nonnegative
        injected (fn [candidate]
                   (if (= {:operations #{} :invariants #{target}}
                          (omitted c candidate))
                     (throw (ex-info "Unrelated checker failure"
                                     {:error/type :other/failure
                                      :reason :broken-backend
                                      :at [:invariants]}))
                     (original-check candidate)))
        r (with-checker injected #(evaluate c))]
    (is (= :blocked (:candidate-status r)))
    (is (= :unavailable
           (get-in r [:selected-release-diagnostic :decisions :candidate
                      :blockers :gesso.release.source/omitted-invariant])))
    (is (= :not-examined (:installation-status r)))
    (assert-no-credential! r)))

(deftest error-with-correct-type-but-wrong-reason-or-location-is-not-detection
  (doseq [data [{:error/type :gesso.assurance.proof.obligation/invalid-obligation-source
                 :reason :not-invariant-count :at [:invariants]}
                {:error/type :gesso.assurance.proof.obligation/invalid-obligation-source
                 :reason :invariant-count :at [:operations]}
                {:error/type :gesso.assurance.proof.operation-set/invalid-operation-set
                 :reason :invalid-operation-sequence :at [:elsewhere]}]]
    (let [c (single-catalog)
          injected (fn [candidate]
                     (if (or (seq (:operations (omitted c candidate)))
                             (seq (:invariants (omitted c candidate))))
                       (throw (ex-info "Forged-looking checker failure" data))
                       (original-check candidate)))
          r (with-checker injected #(evaluate c))]
      (is (= :blocked (:candidate-status r)))
      (is (contains? #{:unavailable :failed}
                     (get-in r [:selected-release-diagnostic :decisions
                                :candidate :blockers
                                :gesso.release.source/omitted-operation])))
      (assert-no-credential! r))))

(deftest forged-unchanged-fingerprint-does-not-satisfy-omission-gate
  (let [c (catalog)
        original-report (original-check c)
        target :request/version-nonnegative
        injected (fn [candidate]
                   (if (= {:operations #{} :invariants #{target}}
                          (omitted c candidate))
                     original-report
                     (original-check candidate)))
        r (with-checker injected #(evaluate c))]
    (is (= :blocked (:candidate-status r)))
    (is (= :failed
           (get-in r [:selected-release-diagnostic :decisions :candidate
                      :blockers :gesso.release.source/omitted-invariant])))
    (assert-no-credential! r)))

(deftest invalid-input-and-original-source-shape-fail-closed
  (doseq [input [nil {} [] :not-a-factory (with-meta {} {:forged true})]]
    (let [e (source-ex-info #(source/assess-original-catalog! input))]
      (is (:release-operation-source/rejection e))
      (is (= :original-catalog-factory-required (:reason e)))))
  (doseq [bad [(assoc (catalog) :declared-operation-ids #{})
               (update (catalog) :operations conj {:id :invalid/incomplete})
               (assoc (catalog) :invariants {})]]
    (let [e (source-ex-info #(evaluate bad))]
      (is (some? e))
      (is (not= :checks-passed-only
                (try (:candidate-status (evaluate bad))
                     (catch clojure.lang.ExceptionInfo _ :rejected))))))
  (let [e (source-ex-info #(source/assess-original-catalog!
                            (fn [] (throw (ex-info "Unavailable source" {:offline true})))))]
    (is (:offline e))))

(deftest substitution-of-original-checker-is-rejected
  (let [e (with-redefs [operation-set/check-operation-set!
                        (fn [_] {:type :forged/report :status :passed})]
            (source-ex-info #(evaluate (catalog))))]
    (is (= :original-operation-checker-replaced (:reason e)))
    (is (:release-operation-source/rejection e))))

(deftest changing-catalog-factory-requires-new-evaluation
  (let [original (catalog)
        alternate (single-catalog)
        now (atom original)
        selected! #(source/assess-original-catalog! (fn [] @now))
        first-report (selected!)]
    (is (= :checks-passed-only (:candidate-status first-report)))
    (reset! now alternate)
    (let [second-report (selected!)]
      (is (= :checks-passed-only (:candidate-status second-report)))
      (is (not= (:selected-catalog-fingerprint first-report)
                (:selected-catalog-fingerprint second-report)))
      (assert-no-credential! first-report)
      (assert-no-credential! second-report))))

(deftest changing-original-source-during-assessment-blocks
  (let [calls (atom 0)
        c (catalog)
        changed (single-catalog)
        r (source/assess-original-catalog!
           (fn [] (if (>= (swap! calls inc) 4) changed c)))]
    (is (>= @calls 4))
    (is (= :blocked (:status r)))
    (is (not= :checks-passed-only (:candidate-status r)))
    (assert-no-credential! r)))

(deftest no-implicit-runtime-effect-through-source-evaluation
  (let [factory-calls (atom 0)
        c (catalog)
        r (source/assess-original-catalog!
           (fn [] (swap! factory-calls inc) c))]
    (is (> @factory-calls 5))
    (is (contains? (get-in r [:selected-release-diagnostic :capabilities])
                   :gesso.release.source/selected-original-catalog))
    (is (not (contains? r :activate!)))
    (is (not (contains? r :install!)))
    (is (not (contains? r :execute!)))
    (is (= :none (:release-authority r)))
    (is (= :none (:runtime-effect-permission r)))
    (assert-no-credential! r)))

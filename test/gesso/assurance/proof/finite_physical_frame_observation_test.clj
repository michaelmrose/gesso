(ns gesso.assurance.proof.finite-physical-frame-observation-test
  "v1416: Permanent adversarial regressions for v1415's finite, caller-supplied
   physical-frame diagnostic. Only enumerated frame-only documents are compared;
   neither source-relative equality nor reported read bases authenticate XTDB
   snapshots, transaction causality, writer closure, or physical frame safety."
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.assurance.proof.finite-physical-frame-observation :as subject]
            [gesso.assurance.proof.protected-write-domain :as domain]
            [gesso.assurance.proof.captured-write-domain-correspondence :as captured]
            [gesso.live.consistency.xtdb :as live]
            [gesso.model.command :as command]
            [gesso.model.tx :as tx]))

(defn- make-command []
  (command/create :document {:xt/id :write :doc/rev 0 :doc/created :t0
                             :doc/updated :t0 :doc/value "SECRET_PLAN_1415"}
                  {:revision-key :doc/rev :created-at-key :doc/created
                   :updated-at-key :doc/updated}))
(defn- dsource
  ([] (dsource [{:target [:document :whole] :fields :all}
                {:target [:document :partial] :fields #{:doc/locked}}]))
  ([frames]
   {:plan {:commands [(make-command)] :emit false}
    :protected-documents (into [{:target [:document :write] :fields :all}] frames)}))
(defn- capture [ops]
  (with-redefs-fn {(ns-resolve 'gesso.live.consistency.xtdb '*submit-tx*)
                   (fn [_ _ _] {:tx-id 1415})}
    #(live/capture-tx-api-calls!
      (fn [] (live/submit-tx! :fake ops)))))
(defn- checked-source [s raw-ops]
  (let [d (domain/derive-protected-write-domain! s)
        c (capture raw-ops)
        source {:domain-source s :domain-report d :capture-report c}]
    {:source source :report (captured/compare-captured-write-domain! source)}))
(defn- env
  ([] (env (dsource)))
  ([s]
   (let [ops (:tx-ops (tx/prepare nil (:plan s)))
         {:keys [source report]} (checked-source s ops)
         entries (get-in source [:domain-report :frame-only-protected-documents])
         docs {[:document :whole] {:doc/secret "PRIVATE_FRAME_1415"
                                   :doc/nested {:x 1}}
               [:document :partial] {:doc/locked 7 :doc/free "FREE_1415"}}
         rows (mapv (fn [{:keys [target]}] {:target target :document (get docs target)})
                    entries)]
     {:captured-source source
      :captured-report report
      :frame-observation
      {:observation-origin :caller-supplied-unattested
       :captured-call-id (get-in source [:capture-report :events 0 :call-id])
       :pre-read-phase :before-attempt
       :post-read-phase :after-attempt
       :reported-pre-read-basis nil
       :reported-post-read-basis nil
       :pre-rows rows
       :post-rows rows}})))
(defn- inspect [e] (subject/compare-finite-physical-frame! e))
(defn- reason [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))
(defn- no-authority [r]
  (is (= :not-established (:physical-frame-for-protected-documents r)))
  (is (= :not-established (:physical-frame-outside-enumeration r)))
  (is (= :not-established (:physical-write-footprint-complete? r)))
  (is (= :not-established (:unobserved-writers-excluded? r)))
  (is (= :not-issued (:certificate r)))
  (is (= :none (:settlement-authority r)))
  (is (= :none (:retry-authority r)))
  (is (= :none (:authority r))))

(deftest matching-bounded-frame-never-establishes-physical-frame
  (let [e (env) r (inspect e)]
    (is (= :bounded-supplied-protected-frame-agrees (:status r)))
    (is (:bounded-adapter-and-frame-agreement? r))
    (is (:supplied-enumerated-frame-equal? r))
    (is (= 2 (:frame-only-target-count r)))
    (is (= 1 (:whole-document-frame-target-count r)))
    (is (= 1 (:partial-field-frame-target-count r)))
    (is (empty? (:changed-frame-target-indices r)))
    (is (= r (subject/require-current-finite-physical-frame! e r)))
    (no-authority r)))

(deftest whole-document-differences-are-visible
  (let [base (env)]
    (doseq [doc [{:doc/secret "PRIVATE_FRAME_1415" :doc/nested {:x 2}}
                 {:doc/secret "PRIVATE_FRAME_1415" :doc/nested {:x 1} :doc/added true}
                 {:doc/secret "PRIVATE_FRAME_1415"}
                 nil
                 {}]]
      (let [r (inspect (assoc-in base [:frame-observation :post-rows 0 :document] doc))]
        (is (= [0] (:changed-frame-target-indices r)))
        (is (= [0] (:changed-whole-document-indices r)))
        (is (= :bounded-supplied-protected-frame-diverges (:status r)))
        (is (false? (:bounded-adapter-and-frame-agreement? r)))
        (no-authority r)))))

(deftest partial-protection-does-not-pretend-to-protect-unlisted-fields
  (let [base (env)
        same (inspect (assoc-in base [:frame-observation :post-rows 1 :document]
                                {:doc/locked 7 :doc/free "CHANGED" :doc/extra :new}))
        changed (inspect (assoc-in base [:frame-observation :post-rows 1 :document]
                                   {:doc/locked 8 :doc/free "FREE_1415"}))
        absent (inspect (assoc-in base [:frame-observation :post-rows 1 :document]
                                  {:doc/free "FREE_1415"}))
        present-nil (inspect (assoc-in base [:frame-observation :post-rows 1 :document]
                                       {:doc/locked nil :doc/free "FREE_1415"}))]
    (is (= :bounded-supplied-protected-frame-agrees (:status same)))
    (is (empty? (:changed-frame-target-indices same)))
    (doseq [r [changed absent present-nil]]
      (is (= [1] (:changed-frame-target-indices r)))
      (is (= [1] (:changed-partial-field-indices r)))
      (is (not (:bounded-adapter-and-frame-agreement? r))))))

(deftest absent-and-present-nil-distinct-and-existence-always-checked
  (let [base (env)
        both-absent (-> base (assoc-in [:frame-observation :pre-rows 1 :document] nil)
                        (assoc-in [:frame-observation :post-rows 1 :document] nil))
        created (assoc-in both-absent [:frame-observation :post-rows 1 :document] {})
        nil-before (-> base (assoc-in [:frame-observation :pre-rows 1 :document] {:doc/locked nil})
                       (assoc-in [:frame-observation :post-rows 1 :document] {}))]
    (is (= :bounded-supplied-protected-frame-agrees (:status (inspect both-absent))))
    (is (= [1] (:document-presence-changed-indices (inspect created))))
    (is (= [1] (:changed-frame-target-indices (inspect nil-before))))
    (is (= :bounded-supplied-protected-frame-diverges (:status (inspect created))))))

(deftest missing-extra-reordered-or-duplicated-rows-cannot-match
  (let [e (env)
        rows (get-in e [:frame-observation :pre-rows])]
    (doseq [bad [(assoc-in e [:frame-observation :pre-rows] (subvec rows 0 1))
                 (assoc-in e [:frame-observation :post-rows] (conj rows (last rows)))
                 (assoc-in e [:frame-observation :pre-rows] (vec (reverse rows)))
                 (assoc-in e [:frame-observation :pre-rows] [(first rows) (first rows)])]]
      (is (#{:incomplete-or-extra-frame-observations
             :reordered-or-unexpected-protected-target}
           (reason #(inspect bad)))))))

(deftest malformed-document-or-contradictory-id-is-rejected
  (let [e (env)]
    (is (= :invalid-observed-document
           (reason #(inspect (assoc-in e [:frame-observation :pre-rows 0 :document] "private")))))
    (is (= :contradictory-observed-target
           (reason #(inspect (assoc-in e [:frame-observation :post-rows 0 :document :xt/id] :wrong)))))
    (is (= :invalid-map-shape
           (reason #(inspect (update-in e [:frame-observation :post-rows 0] assoc :extra true)))))))

(deftest only-the-reported-single-adapter-call-can-bind-observations
  (let [e (env)]
    (is (= :unbound-or-unsupported-frame-observation
           (reason #(inspect (assoc-in e [:frame-observation :captured-call-id] (random-uuid))))))
    (is (= :unbound-or-unsupported-frame-observation
           (reason #(inspect (assoc-in e [:frame-observation :pre-read-phase] :after-attempt)))))
    (is (= :unbound-or-unsupported-frame-observation
           (reason #(inspect (assoc-in e [:frame-observation :reported-post-read-basis] "  ")))))
    (is (= :bounded-supplied-protected-frame-agrees
           (:status (inspect (-> e
                                 (assoc-in [:frame-observation :reported-pre-read-basis] 23)
                                 (assoc-in [:frame-observation :reported-post-read-basis] (random-uuid)))))))))

(deftest mismatched-adapter-plan-cannot-generate-positive-joint-agreement
  (let [s (dsource)
        ops (:tx-ops (tx/prepare nil (:plan s)))
        {:keys [source report]} (checked-source s (conj ops ["ASSERT TRUE"]))
        e (-> (env s)
              (assoc :captured-source source :captured-report report)
              (assoc-in [:frame-observation :captured-call-id]
                        (get-in source [:capture-report :events 0 :call-id])))
        r (inspect e)]
    (is (= :captured-operations-diverge (:status report)))
    (is (:supplied-enumerated-frame-equal? r))
    (is (= :adapter-plan-correspondence-unavailable (:status r)))
    (is (false? (:bounded-adapter-and-frame-agreement? r)))))

(deftest zero-frame-domain-is-not-a-positive-result
  (let [e (env (dsource [])) r (inspect e)]
    (is (= 0 (:frame-only-target-count r)))
    (is (not (:supplied-enumerated-frame-equal? r)))
    (is (not (:bounded-adapter-and-frame-agreement? r)))
    (is (= :no-frame-only-targets-enumerated (:status r)))))

(deftest stale-and-forged-reports-rejected
  (let [e (env) r (inspect e)]
    (doseq [bad [(assoc r :status :certified)
                 (assoc r :changed-frame-target-indices [0])
                 (assoc-in r [:source :frame-observation :pre-rows 0 :document :doc/secret] "FORGED")]]
      (is (= :stale-or-forged-frame-report
             (reason #(subject/require-current-finite-physical-frame! e bad)))))))

(deftest redaction-excludes-source-and-read-basis
  (let [e (-> (env)
              (assoc-in [:frame-observation :reported-pre-read-basis] "PRIVATE_BASIS_1415"))
        r (inspect e) summary (subject/redacted-summary r)
        text (pr-str summary)]
    (is (not (contains? summary :source)))
    (is (not (re-find #"PRIVATE_FRAME_1415|FREE_1415|PRIVATE_BASIS_1415|SECRET_PLAN_1415|:doc/locked|:doc/secret" text)))
    (no-authority summary)))

(deftest upstream-validator-replacement-rejected
  (let [e (env)
        check-var (ns-resolve 'gesso.assurance.proof.captured-write-domain-correspondence
                              'require-current-captured-write-domain!)]
    (is (= :substituted-upstream-checker
           (reason #(with-redefs-fn {check-var (fn [& _] :fake)}
                      (fn [] (inspect e))))))))

(deftest difference-counts-and-indices-are-per-protection-domain
  (let [e (env)
        changed (-> e
                    (assoc-in [:frame-observation :post-rows 0 :document]
                              {:doc/secret "NEW" :doc/nested {:x 2} :doc/new true})
                    (assoc-in [:frame-observation :post-rows 1 :document]
                              {:doc/locked nil :doc/free "NEW"}))
        r (inspect changed)]
    (is (= [0 1] (:changed-frame-target-indices r)))
    (is (= [0] (:changed-whole-document-indices r)))
    (is (= [1] (:changed-partial-field-indices r)))
    (is (= 4 (:different-protected-field-count r)))
    (is (= :bounded-supplied-protected-frame-diverges (:status r)))
    (no-authority r)))

(deftest changed-unprotected-fields-do-not-affect-partial-frame
  (let [e (env)
        post (get-in e [:frame-observation :post-rows 1 :document])
        r (inspect (assoc-in e [:frame-observation :post-rows 1 :document]
                             (-> post (dissoc :doc/free)
                                 (assoc :doc/aux {:unexpected [1 2]}))))]
    (is (= :bounded-supplied-protected-frame-agrees (:status r)))
    (is (= 0 (:different-protected-field-count r)))
    (is (:bounded-adapter-and-frame-agreement? r))
    (no-authority r)))

(deftest partial-protection-preserves-document-existence
  (let [e (env)
        pre-nil (assoc-in e [:frame-observation :pre-rows 1 :document] nil)
        create (assoc-in pre-nil [:frame-observation :post-rows 1 :document] {})
        delete (assoc-in e [:frame-observation :post-rows 1 :document] nil)]
    (doseq [bad [create delete]]
      (let [r (inspect bad)]
        (is (= [1] (:document-presence-changed-indices r)))
        (is (= [1] (:changed-partial-field-indices r)))
        (is (= :bounded-supplied-protected-frame-diverges (:status r)))
        (no-authority r)))))

(deftest nil-and-absent-protected-keys-remain-distinct
  (let [e (env)
        both-nil (-> e
                     (assoc-in [:frame-observation :pre-rows 1 :document] {:doc/locked nil})
                     (assoc-in [:frame-observation :post-rows 1 :document] {:doc/locked nil}))
        both-absent (-> e
                        (assoc-in [:frame-observation :pre-rows 1 :document] {:doc/free :old})
                        (assoc-in [:frame-observation :post-rows 1 :document] {:doc/free :new}))
        removed (assoc-in both-nil [:frame-observation :post-rows 1 :document] {})]
    (is (= :bounded-supplied-protected-frame-agrees (:status (inspect both-nil))))
    (is (= :bounded-supplied-protected-frame-agrees (:status (inspect both-absent))))
    (is (= [1] (:changed-frame-target-indices (inspect removed))))
    (is (= 1 (:different-protected-field-count (inspect removed))))))

(deftest absent-snapshot-row-is-not-explicitly-absent-document
  (let [e (env)
        absent-doc (assoc-in e [:frame-observation :post-rows 0 :document] nil)
        absent-row (update-in e [:frame-observation :post-rows] pop)
        complete-absence (-> e
                             (assoc-in [:frame-observation :pre-rows 0 :document] nil)
                             (assoc-in [:frame-observation :post-rows 0 :document] nil))]
    (is (= :bounded-supplied-protected-frame-diverges (:status (inspect absent-doc))))
    (is (= :incomplete-or-extra-frame-observations (reason #(inspect absent-row))))
    (is (= :bounded-supplied-protected-frame-agrees (:status (inspect complete-absence))))))

(deftest extra-target-rows-even-with-matching-documents-cannot-pass
  (let [e (env)
        extra {:target [:document :not-declared] :document {:doc/locked 7}}
        pre (get-in e [:frame-observation :pre-rows])
        post (get-in e [:frame-observation :post-rows])]
    (doseq [bad [(assoc-in e [:frame-observation :pre-rows] (conj pre extra))
                 (assoc-in e [:frame-observation :post-rows] (conj post extra))
                 (-> e
                     (assoc-in [:frame-observation :pre-rows] (conj pre extra))
                     (assoc-in [:frame-observation :post-rows] (conj post extra)))]]
      (is (= :incomplete-or-extra-frame-observations (reason #(inspect bad)))))))

(deftest mismatched-targets-and-claimed-document-ids-rejected
  (let [e (env)]
    (is (= :reordered-or-unexpected-protected-target
           (reason #(inspect (assoc-in e [:frame-observation :pre-rows 0 :target]
                                      [:document :partial])))))
    (is (= :contradictory-observed-target
           (reason #(inspect (assoc-in e [:frame-observation :pre-rows 1 :document :xt/id]
                                      :not-partial)))))
    ;; A consistent :xt/id is not part of the declared partial field set.
    (is (= :bounded-supplied-protected-frame-agrees
           (:status (inspect (assoc-in e [:frame-observation :post-rows 1 :document :xt/id]
                                      :partial)))))))

(deftest malformed-envelope-and-metadata-are-rejected
  (let [e (env)
        basic (get-in e [:frame-observation])]
    (doseq [bad [(assoc e :untrusted true)
                 (dissoc e :captured-report)
                 (assoc e :frame-observation (assoc basic :certificate :certified))
                 (assoc e :frame-observation (dissoc basic :observation-origin))
                 (with-meta e {:approved true})
                 (assoc-in e [:frame-observation :pre-rows 0] (with-meta
                                                                 (get-in e [:frame-observation :pre-rows 0])
                                                                 {:trusted true}))]]
      (is (= :invalid-map-shape (reason #(inspect bad)))))
    (is (= :incomplete-or-extra-frame-observations
           (reason #(inspect (assoc-in e [:frame-observation :pre-rows]
                                        (with-meta (get-in e [:frame-observation :pre-rows])
                                          {:trusted true}))))))))

(deftest unsupported-observed-document-shapes-are-rejected
  (let [e (env)]
    (doseq [bad ["secret" [] #{:doc/locked} {"field" 7}
                 (with-meta {:doc/secret :value} {:trusted true})]]
      (is (= :invalid-observed-document
             (reason #(inspect (assoc-in e [:frame-observation :post-rows 0 :document] bad))))))))

(deftest read-tokens-and-reported-phases-do-not-authenticate-order
  (let [e (env)
        basis-values [[0 100] [200 1] [(random-uuid) (random-uuid)] ["opaque-pre" "opaque-post"]]]
    (doseq [[before after] basis-values]
      (let [r (inspect (-> e
                           (assoc-in [:frame-observation :reported-pre-read-basis] before)
                           (assoc-in [:frame-observation :reported-post-read-basis] after)))]
        (is (= :bounded-supplied-protected-frame-agrees (:status r)))
        (is (false? (:pre-read-basis-authenticated? r)))
        (is (false? (:post-read-basis-authenticated? r)))
        (is (false? (:snapshot-order-authenticated? r)))
        (is (= :not-established (:snapshot-causality r)))
        (no-authority r)))))

(deftest bogus-observation-origin-or-call-binding-rejected
  (let [e (env)]
    (doseq [[path value] [[[:frame-observation :observation-origin] :xtdb-authenticated]
                          [[:frame-observation :captured-call-id] :not-a-uuid]
                          [[:frame-observation :post-read-phase] :before-attempt]
                          [[:frame-observation :pre-read-phase] :outside-attempt]
                          [[:frame-observation :reported-pre-read-basis] {:snapshot :forged}]
                          [[:frame-observation :reported-post-read-basis] " "]]]
      (is (= :unbound-or-unsupported-frame-observation
             (reason #(inspect (assoc-in e path value))))))))

(deftest stale-upstream-or-replaced-checkers-fail-before-positive-report
  (let [e (env)
        stale (assoc-in e [:captured-report :status] :not-a-real-capture-status)
        upstream-var (ns-resolve 'gesso.assurance.proof.captured-write-domain-correspondence
                                 'require-current-captured-write-domain!)]
    (is (some? (reason #(inspect stale))))
    (is (= :substituted-upstream-checker
           (reason #(with-redefs-fn {upstream-var (fn [& _] (:captured-report e))}
                      (fn [] (inspect e))))))))

(deftest forged-report-cannot-assert-certified-physical-frame
  (let [e (env) r (inspect e)]
    (doseq [changed [(assoc r :physical-frame-for-protected-documents :established)
                     (assoc r :pre-read-origin-authenticated? true)
                     (assoc r :writer-path-closure :established)
                     (assoc r :settlement-authority :granted)
                     (assoc r :certificate :issued)
                     (assoc r :proof-status :certified)
                     (assoc-in r [:source :frame-observation :pre-rows 0 :document :doc/secret]
                               "FORGED_IN_REPORT_1416")
                     (dissoc r :source)
                     (with-meta r {:certified true})]]
      (is (= :stale-or-forged-frame-report
             (reason #(subject/require-current-finite-physical-frame! e changed)))))))

(deftest redacted-report-and-rejection-hide-private-documents
  (let [e (-> (env)
              (assoc-in [:frame-observation :reported-pre-read-basis] "PRIVATE_READ_BASIS_1416"))
        r (inspect e)
        summary (subject/redacted-summary r)
        poison "PRIVATE_1416_DO_NOT_LOG"
        malformed (assoc-in e [:frame-observation :post-rows 0 :document] poison)
        rejection (try (inspect malformed) nil
                       (catch clojure.lang.ExceptionInfo ex (ex-data ex)))
        printed (pr-str summary rejection)]
    (is (not (contains? summary :source)))
    (is (not (re-find #"PRIVATE_FRAME_1415|FREE_1415|SECRET_PLAN_1415|PRIVATE_READ_BASIS_1416|PRIVATE_1416_DO_NOT_LOG|:doc/locked|:doc/secret"
                      printed)))
    (is (= :invalid-observed-document (:reason rejection)))
    (is (vector? (:at rejection)))
    (no-authority summary)))

(deftest empty-frame-domain-stays-not-applicable-even-when-adapter-matches
  (let [e (env (dsource [])) r (inspect e)]
    (is (= 0 (:frame-only-target-count r)))
    (is (= :bounded-captured-adapter-operations-agree (:captured-domain-status r)))
    (is (= :no-frame-only-targets-enumerated (:status r)))
    (is (false? (:bounded-adapter-and-frame-agreement? r)))
    (is (false? (:supplied-enumerated-frame-equal? r)))
    (no-authority r)))

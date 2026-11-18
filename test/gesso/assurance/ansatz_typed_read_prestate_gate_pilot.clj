(ns gesso.assurance.ansatz-typed-read-prestate-gate-pilot
  "Opt-in v1754: checked, source-derived PRE-invocation read premises.

   Build on the SAME v1750 theorem and v1753 verified transition bridge.
   A caller can check the original typed prestate, exact inputs, operation
   guard and every tracked source-read invariant BEFORE any poststate exists.
   The observation wrapper RECHECKS the current values rather than trusting
   a copyable, possibly stale earlier preflight report.

   This is an abstract pre-invocation screening API, not installation,
   actual model/FX invocation admission, transaction isolation, XTDB
   precondition enforcement, a certificate or runtime authority. Passing
   one snapshot never authorizes committing a subsequent changed snapshot."
  (:require [ansatz.core :as ansatz]
            [clojure.test :refer [deftest is run-tests testing]]
            [gesso.assurance.ansatz-typed-read-transition-bridge-pilot]
            [gesso.assurance.ansatz-typed-read-induction-pilot]
            [gesso.assurance.ansatz-typed-read-pilot]
            [gesso.assurance.rule-eval :as rule-eval]))

(def ^:private bridge-ns 'gesso.assurance.ansatz-typed-read-transition-bridge-pilot)
(def ^:private induction-ns 'gesso.assurance.ansatz-typed-read-induction-pilot)
(def ^:private read-ns 'gesso.assurance.ansatz-typed-read-pilot)

(defn- reject! [reason details]
  (throw (ex-info "Unqualified abstract typed-read prestate" {:reason reason :details details})))

(defn- pilot! [ns-name sym & args]
  (let [v (ns-resolve ns-name sym)]
    (when-not v (reject! :missing-source-contract {:ns ns-name :symbol sym}))
    (apply @v args)))

(defn- original-premise-keys! [model]
  (let [sources (get-in model [:source :sources])
        invariants (get-in model [:source :invariants])
        expected (vec (for [src sources
                            [invariant-id inv] (sort-by key invariants)
                            :let [field (:field inv)
                                  assignment (get-in src [:semantics :assignments field])]
                            :when (= :read (first assignment))]
                        {:operation-id (:id src)
                         :invariant-id invariant-id
                         :field field
                         :reader (second assignment)
                         :reader-origin (if (contains? (get-in src [:source :input-types])
                                                       (second assignment))
                                          :input :prestate)}))
        declared (get-in model [:theorem :read-premises])
        derived (mapv #(select-keys % [:operation-id :invariant-id :reader
                                       :reader-origin]) expected)
        claimed (mapv #(select-keys % [:operation-id :invariant-id :reader
                                       :reader-origin]) declared)
        pairs (mapv (juxt :operation-id :invariant-id) expected)
        trace-sources (get-in model [:source :sources])]
    (when-not (and (seq expected)
                   (= (count pairs) (count (set pairs)))
                   (= (frequencies derived) (frequencies claimed))
                   (= (count declared) (count expected))
                   (= (set (map :operation-id expected))
                      (set (map :operation-id (:operations model))))
                   (= (set (map :id trace-sources))
                      (set (map :operation-id (:operations model))))
                   (= (set (map :id trace-sources))
                      (set (map :operation-id (:operations model))))
                   (every? (fn [src]
                             (= (:transition-fingerprint src)
                                (:transition-fingerprint
                                 (first (filter #(= (:operation-id %) (:id src))
                                                (:operations model))))))
                           trace-sources))
      ;; A checked theorem does not certify an unknown or omitted source pair.
      (reject! :read-premise-inventory-not-source-derived
               {:expected derived :declared claimed}))
    expected))

(defn- source-operation! [model id]
  (let [sources (get-in model [:source :sources])
        source (first (filter #(= id (:id %)) sources))]
    (when-not source (reject! :undeclared-operation {:operation id}))
    source))

(defn- screen! [model by-operation id before inputs]
  (let [{:keys [source]} (source-operation! model id)
        state-types (:state-types source)
        input-types (:input-types source)
        all-types (merge state-types input-types)
        guard (:guard source)
        guard-reads (:readers guard)
        available (merge before inputs)
        invariants (get-in model [:source :invariants])
        premises (get by-operation id [])]
    (pilot! read-ns 'projection! state-types :before before)
    (pilot! read-ns 'projection! input-types :inputs inputs)
    (when-not (= (set (keys state-types)) (set (keys before)))
      (reject! :state-fields-not-exact {:operation id}))
    (doseq [[invariant-id invariant] invariants]
      (when-not (pilot! read-ns 'source-predicate-holds?
                        invariant (get before (:field invariant)))
        (reject! :source-prestate-invariant-failed
                 {:operation id :invariant invariant-id})))
    (let [result (rule-eval/evaluate-rule!
                  guard (select-keys all-types guard-reads)
                  (select-keys available guard-reads))]
      (when-not (and (= :syntax-and-types (:checking result))
                     (= :pure-interpreted (:evaluation result))
                     (= :none (:authority result)))
        (reject! :unqualified-original-guard-evaluation {:operation id}))
      (when-not (true? (:value result))
        (reject! :operation-guard-false {:operation id})))
    (doseq [{:keys [invariant-id reader reader-origin]} premises]
      (let [invariant (get invariants invariant-id)
            value (get (if (= :input reader-origin) inputs before) reader)]
        (when-not (and invariant
                       (pilot! read-ns 'source-predicate-holds? invariant value))
          (reject! :source-read-premise-failed
                   {:operation id :invariant invariant-id
                    :reader reader :origin reader-origin}))))
    {:operation-id id
     :catalog-fingerprint (:catalog-fingerprint model)
     :checked-read-premises (count premises)
     :guard-holds? true
     :state-invariants-checked (count invariants)
     :checking :abstract-pre-invocation-source-read-premises
     :usable-for-physical-commit? false
     :authority :none :admission :not-admitted :certificate :not-issued}))

(defn- checked-prestate-boundary! [catalog]
  (let [{:keys [model] :as checked} (pilot! bridge-ns 'check-bridge! catalog)
        original (original-premise-keys! model)
        by-operation (group-by :operation-id original)
        downstream (pilot! bridge-ns 'audited-boundary! catalog)
        ids (set (map :id (get-in model [:source :sources])))]
    (when-not (and (every? ids (keys by-operation))
                   (= (:catalog-fingerprint model)
                      (:catalog-fingerprint downstream))
                   (= :none (:authority checked) (:authority downstream))
                   (= :not-admitted (:admission checked)
                      (:admission downstream)))
      (reject! :prestate-bridge-not-current {:ids ids}))
    {:checking :conditional-pre-invocation-abstract-read-gate
     :catalog-fingerprint (:catalog-fingerprint model)
     :read-premise-count (count original)
     :unproved-read-targets (:unproved-read-targets downstream)
     :physical-prestate-lock :not-established
     :actual-operation-admission :not-established
     :open-assumptions (:open-assumptions downstream)
     :authority :none :admission :not-admitted :certificate :not-issued
     :check-prestate! (fn [id before inputs]
                        (screen! model by-operation id before inputs))
     :observe! (fn [id before inputs after]
                 ;; A preflight result is COPYABLE and never trusted as a token.
                 ;; Check the actual supplied observation snapshot again.
                 (screen! model by-operation id before inputs)
                 ((:observe! downstream) id before inputs after))}))

(defn- catalog [] (pilot! induction-ns 'ticket-catalog))
(defn- witness [] (pilot! read-ns 'assign-witness))
(defn- reason [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))

(deftest source-premises-screened-before-there-is-a-poststate
  (let [gate (checked-prestate-boundary! (catalog))
        [before inputs after] (witness)
        assign ((:check-prestate! gate) :ticket/assign before inputs)
        restore (assoc before :ticket/status :claimed)]
    (is (= 5 (:read-premise-count gate)))
    (is (= 3 (:checked-read-premises assign)))
    (is (= 2 (:checked-read-premises
              ((:check-prestate! gate) :ticket/restore restore {}))))
    (is (= :abstract-pre-invocation-source-read-premises (:checking assign)))
    (is (false? (:usable-for-physical-commit? assign)))
    (is (= :not-admitted (:admission assign)))
    (is (= :none (:authority gate) (:authority assign)))
    (is (:step-holds? ((:observe! gate) :ticket/assign before inputs after)))
    (is (= #{[:ticket/assign :ticket/owner]} (:unproved-read-targets gate)))))

(deftest invalid-input-types-and-guards-rejected-without-poststate
  (let [screen (:check-prestate! (checked-prestate-boundary! (catalog)))
        [before inputs _] (witness)]
    (is (= :undeclared-operation
           (reason #(screen :ticket/unknown before inputs))))
    (is (= :operation-guard-false
           (reason #(screen :ticket/assign (assoc before :ticket/status :claimed) inputs))))
    (doseq [invalid [(assoc inputs :input/count 2.5)
                     (assoc inputs :input/actor nil)
                     (dissoc inputs :input/count)
                     (assoc inputs :input/extra 3)]]
      (is (= :invalid-typed-projection
             (reason #(screen :ticket/assign before invalid)))))))

(deftest source-read-premises-are-not-implied-by-type-correctness
  (let [screen (:check-prestate! (checked-prestate-boundary! (catalog)))
        [before inputs _] (witness)
        claimed (assoc before :ticket/status :claimed)]
    (is (= :source-read-premise-failed
           (reason #(screen :ticket/assign before
                            (assoc inputs :input/count 8)))))
    ;; The default domain includes both :web and :phone, so it is
    ;; deliberately NOT treated as a proper-subset restriction.
    (is (= 3 (:checked-read-premises
              (screen :ticket/assign before
                      (assoc inputs :input/channel :phone)))))
    (is (= :source-read-premise-failed
           (reason #(screen :ticket/restore
                            (assoc claimed :ticket/backup 8) {}))))
    (is (= :source-prestate-invariant-failed
           (reason #(screen :ticket/assign
                            (assoc before :ticket/count -1) inputs))))))

(deftest proper-enum-subset-premise-is-checked-before-observation
  (let [narrow (-> (catalog)
                   (assoc-in [:invariants :ticket/channel-domain :expression 2 1]
                             #{:web})
                   (assoc-in [:initial-condition :expression 3 2 1]
                             #{:web}))
        screen (:check-prestate! (checked-prestate-boundary! narrow))
        [before inputs _] (witness)
        permitted (assoc before :ticket/channel :web)]
    (is (= :source-prestate-invariant-failed
           (reason #(screen :ticket/assign before inputs))))
    (is (= :source-read-premise-failed
           (reason #(screen :ticket/assign permitted
                            (assoc inputs :input/channel :phone)))))
    (is (= 3 (:checked-read-premises
              (screen :ticket/assign permitted inputs))))))

(deftest stale-copied-preflight-does-not-authorize-later-observation
  (let [gate (checked-prestate-boundary! (catalog))
        [before inputs after] (witness)
        _ ((:check-prestate! gate) :ticket/assign before inputs)]
    (is (= :source-read-premise-failed
           (reason #((:observe! gate) :ticket/assign before
                     (assoc inputs :input/count 8)
                     (assoc after :ticket/count 8)))))
    (is (= :operation-guard-false
           (reason #((:observe! gate) :ticket/assign
                     (assoc before :ticket/status :claimed) inputs after))))
    (is (= :original-observation-not-accepted
           (reason #((:observe! gate) :ticket/assign
                     before inputs (assoc after :ticket/owner :user/incorrect)))))
    (is (:step-holds? ((:observe! gate) :ticket/assign before inputs after)))))

(deftest theorem-premise-inventory-cannot-be-shortened-after-proof
  (let [original (ns-resolve bridge-ns 'check-bridge!)
        base @original
        modified (fn [catalog]
                   (update-in (base catalog) [:model :theorem :read-premises] pop))]
    (is (= :read-premise-inventory-not-source-derived
           (reason #(with-redefs-fn {original modified}
                      (fn [] (checked-prestate-boundary! (catalog)))))))))

(deftest signed-bounds-screen-the-same-read-values
  (let [narrow (-> (catalog)
                   (assoc-in [:invariants :ticket/count-min :expression]
                             [:>= [:read :ticket/count] [:literal -2]])
                   (assoc-in [:initial-condition :expression 1]
                             [:>= [:read :ticket/count] [:literal -2]])
                   (assoc-in [:invariants :ticket/count-max :expression]
                             [:<= [:read :ticket/count] [:literal 3]])
                   (assoc-in [:initial-condition :expression 2]
                             [:<= [:read :ticket/count] [:literal 3]]))
        gate (checked-prestate-boundary! narrow)
        [before inputs _] (witness)
        claimed (assoc before :ticket/status :claimed)]
    (is (= 3 (:checked-read-premises
              ((:check-prestate! gate) :ticket/assign before inputs))))
    (is (= :source-read-premise-failed
           (reason #((:check-prestate! gate) :ticket/assign before
                     (assoc inputs :input/count 4)))))
    (is (= 2 (:checked-read-premises
              ((:check-prestate! gate) :ticket/restore
               (assoc claimed :ticket/backup -1) {}))))
    (is (= :source-read-premise-failed
           (reason #((:check-prestate! gate) :ticket/restore
                     (assoc claimed :ticket/backup 5) {}))))))

(defn -main [& _]
  (binding [ansatz/*verbose* false] (ansatz/load-init!))
  (let [{:keys [fail error]}
        (run-tests 'gesso.assurance.ansatz-typed-read-prestate-gate-pilot)]
    (when (pos? (+ fail error)) (System/exit 1))))

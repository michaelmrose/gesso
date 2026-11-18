(ns gesso.assurance.ansatz-mixed-initialization-pilot
  "Opt-in v1745: initialization obligations for ordinary mixed Gesso catalogs.

   For the exact ORIGINAL typed :initial-condition conjunction, require every
   declared integer-bound or nominal finite-enum invariant exactly once.
   Derive kernel-checked implications to each invariant and a concrete
   satisfiability witness for the tracked state fields, using the same
   source-bound Int/enum translation as the v1744 preservation compiler.
   Recheck the complete preservation proof product before composing reports.

   The initial condition is a DECLARED PREDICATE, not an actual installed
   initial XTDB state. These are conditional abstract proofs; source/enum
   translation, interpreter correspondence, physical writer completeness,
   and initial DB state remain open. No admission, certification or authority.
   Ordinary downstream Clojure code requires no proof annotations."
  (:require [ansatz.core :as ansatz]
            [ansatz.kernel.env :as kernel]
            [clojure.set :as set]
            [clojure.test :refer [deftest is run-tests testing]]
            [gesso.assurance.ansatz-literal-preservation-pilot]
            [gesso.assurance.ansatz-reservation-pilot]
            [gesso.assurance.example.single-node-proving :as example]
            [gesso.assurance.rule-type :as rule-type]
            [gesso.assurance.subject :as subject]))

(def ^:private preservation-ns 'gesso.assurance.ansatz-literal-preservation-pilot)
(def ^:private proof-ns 'gesso.assurance.ansatz-reservation-pilot)
(def ^:private open-assumptions
  #{:actual-initial-database-state-satisfies-declaration
    :gesso-source-to-ansatz-translation-correctness
    :finite-enum-int-encoding-correspondence
    :actual-transitions-refine-abstract-guarded-replacements
    :physical-writer-closure-and-source-currentness
    :ansatz-kernel-and-permitted-axioms})

(defn- reject! [reason details]
  (throw (ex-info "Unsupported mixed Ansatz initialization evidence"
                  {:reason reason :details details})))

(defn- pilot! [n function & args]
  (let [v (ns-resolve n function)]
    (when-not v (reject! :missing-pilot-contract {:function function}))
    (apply @v args)))

(defn- conjunction [forms]
  (when-not (<= 2 (count forms) 12)
    (reject! :unsupported-initial-conjunction-size {:count (count forms)}))
  (reduce (fn [right left] (list 'And left right))
          (last forms) (reverse (butlast forms))))

(defn- pattern [names]
  (reduce (fn [right left] [left right])
          (last names) (reverse (butlast names))))

(defn- initial-source! [catalog]
  (let [base (dissoc catalog :initial-condition)
        {:keys [identity obligations]} (pilot! preservation-ns 'source-obligations! base)
        initial (:initial-condition catalog)
        invariants (:invariants base)
        originals (mapv (fn [id]
                          (pilot! preservation-ns 'invariant! id (get invariants id)))
                        (:invariant-ids identity))
        expressions (mapv (comp :expression :rule) originals)
        field-names (vec (sort (set (map :field originals))))
        reader-set (set field-names)
        expr (:expression initial)
        leaves (when (and (vector? expr) (= :and (first expr)))
                 (vec (rest expr)))
        field-types (select-keys (get-in base [:operations 0 :construction :state-types])
                                 reader-set)
        _ (when-not (and (= #{:readers :expression} (set (keys initial)))
                         (= reader-set (:readers initial))
                         (<= 2 (count originals) 12)
                         (= (count originals) (count (set expressions)))
                         (= (frequencies expressions) (frequencies leaves))
                         (= reader-set (set (keys field-types))))
            (reject! :unsupported-initial-condition
                     {:initial initial :invariant-expressions expressions}))
        _ (doseq [op (:operations base)]
            (when-not (= field-types
                         (select-keys (get-in op [:construction :state-types]) reader-set))
              (reject! :inconsistent-initial-field-types {:operation (:id op)})))
        _ (rule-type/check-rule-type! initial field-types)
        ;; Source expressions are unique by the check above: resolve the
        ;; ORIGINAL expression order back to invariant IDs, not map order.
        by-expression (into {} (map (juxt (comp :expression :rule) :invariant-id)
                                    originals))
        ordered-ids (mapv by-expression leaves)
        invariants-by-id (into {} (map (juxt :invariant-id clojure.core/identity) originals))
        field-vars (zipmap field-names (map #(symbol (str "s" %)) (range)))
        full-id (:fingerprint (subject/describe-subject
                               {:kind :rule
                                :id :gesso.assurance.ansatz-pilot/mixed-initialization
                                :interface catalog}))]
    (when-not (= (set ordered-ids) (set (:invariant-ids identity)))
      (reject! :incomplete-initialization-coverage {:ids ordered-ids}))
    {:catalog-fingerprint full-id
     :preservation-fingerprint (:fingerprint identity)
     :preservation-obligation-count (count obligations)
     :source-ordered-ids ordered-ids
     :invariants invariants-by-id
     :field-names field-names
     :field-vars field-vars
     :state-types field-types
     :initial-source initial}))

(defn- source-proposition! [source invariant-id field-values]
  (let [{:keys [field kind op bound members]} (get-in source [:invariants invariant-id])
        value (get field-values field)]
    (when (nil? value)
      (reject! :missing-initial-proposition-field {:invariant-id invariant-id}))
    (case kind
      :signed-int-bound (pilot! preservation-ns 'comparison op value bound)
      :finite-enum-membership
      (let [enum (pilot! preservation-ns 'state-field-type!
                         (get-in source [:invariants invariant-id])
                         {:state-types (:state-types source)})]
        (pilot! preservation-ns 'enum-membership (:codes enum) members value))
      (reject! :unsupported-init-invariant {:invariant-id invariant-id}))))

(defn- tracked-witness! [source]
  ;; A witness for the CLOSED TRACKED projection, not a claim that this
  ;; map is an installed initial DB record or a complete Clojure state.
  (into (sorted-map)
        (for [field (:field-names source)]
          (let [rules (filter #(= field (:field %))
                              (vals (:invariants source)))
                kinds (set (map :kind rules))
                value (cond
                        (= kinds #{:signed-int-bound})
                        (let [lows (map :bound (filter #(= :>= (:op %)) rules))
                              highs (map :bound (filter #(= :<= (:op %)) rules))
                              low (when (seq lows) (apply max lows))
                              high (when (seq highs) (apply min highs))]
                          (when (and low high (> low high))
                            (reject! :unsatisfiable-initial-integer-bounds
                                     {:field field :lower low :upper high}))
                          (or low high 0))

                        (= kinds #{:finite-enum-membership})
                        (let [allowed (apply set/intersection
                                             (map :members rules))]
                          (when-not (seq allowed)
                            (reject! :unsatisfiable-initial-enum-intersection
                                     {:field field :invariants (map :invariant-id rules)}))
                          (first (sort-by str allowed)))
                        :else (reject! :incompatible-initial-field-invariants
                                       {:field field :kinds kinds}))]
            [field value]))))

(defn- witness-proposition-value [source field value]
  (let [desc (get-in source [:state-types field])]
    (if (= :enum (:type desc))
      (let [enc (pilot! preservation-ns 'state-field-type!
                        {:kind :finite-enum-membership
                         :field field
                         :members (:values desc)
                         :rule {:readers #{field}
                                :expression [:in [:read field]
                                             [:literal (:values desc)]]}}
                        {:state-types (:state-types source)})]
        (get (:codes enc) value))
      value)))

(defn- witness-leaf-tactics [source invariant-id witness]
  (let [{:keys [kind field members]} (get-in source [:invariants invariant-id])]
    (case kind
      :signed-int-bound '[(omega)]
      :finite-enum-membership
      (vec (conj (vec (butlast
                       (pilot! preservation-ns 'enum-write-script members (get witness field))))
                 '(rfl)))
      (reject! :unsupported-init-proof-kind {:invariant-id invariant-id}))))

(defn- source-initialization-obligations! [catalog]
  (let [source (initial-source! catalog)
        ordered (:source-ordered-ids source)
        field-vars (:field-vars source)
        abstract-forms (mapv #(source-proposition! source % field-vars) ordered)
        initial-form (conjunction abstract-forms)
        proof-names (mapv #(symbol (str "h" %)) (range (count ordered)))
        telescope (vec (concat (mapcat (fn [field] [(get field-vars field) ':- 'Int])
                                       (:field-names source))
                               ['hinit ':- initial-form]))
        projection (mapv (fn [i id]
                           {:obligation-kind :mixed-initial-entails-invariant
                            :invariant-id id
                            :catalog-fingerprint (:catalog-fingerprint source)
                            :preservation-fingerprint (:preservation-fingerprint source)
                            :parameters telescope
                            :goal-form (nth abstract-forms i)
                            :proof-script [(list 'rcases 'hinit (pattern proof-names))
                                           (list 'exact (nth proof-names i))]})
                         (range (count ordered)) ordered)
        witness (tracked-witness! source)
        literal-values (into {} (for [[field value] witness]
                                  [field (witness-proposition-value source field value)]))
        witness-forms (mapv #(source-proposition! source % literal-values) ordered)
        constructor-script (vec
                            (mapcat (fn [i id]
                                      (concat (when (< i (dec (count ordered)))
                                                ['(constructor)])
                                              (witness-leaf-tactics source id witness)))
                                    (range (count ordered)) ordered))
        nonvacuity {:obligation-kind :mixed-initial-predicate-inhabited
                    :invariant-id nil
                    :catalog-fingerprint (:catalog-fingerprint source)
                    :preservation-fingerprint (:preservation-fingerprint source)
                    :witness-projection witness
                    :parameters []
                    :goal-form (conjunction witness-forms)
                    :proof-script constructor-script}]
    {:source source :obligations (conj projection nonvacuity)}))

(defn- check-initialization! [catalog]
  (let [{:keys [source obligations]} (source-initialization-obligations! catalog)
        checked (mapv #(pilot! proof-ns 'check-obligation! %) obligations)]
    (when-not (and (= (inc (count (:source-ordered-ids source))) (count checked))
                   (every? (fn [{:keys [goal proof dependencies description]}]
                             (and (= (:catalog-fingerprint source)
                                     (:catalog-fingerprint description))
                                  (= (:preservation-fingerprint source)
                                     (:preservation-fingerprint description))
                                  (kernel/verifies? (ansatz/env) goal proof)
                                  (= :ansatz-reservation-pilot/lean-core-axioms-v1
                                     (:policy dependencies))
                                  (empty? (:non-safe dependencies))
                                  (empty? (:unresolved dependencies)))) checked))
      (reject! :unqualified-mixed-initialization {:count (count checked)}))
    {:source-fingerprint (:catalog-fingerprint source)
     :preservation-fingerprint (:preservation-fingerprint source)
     :invariant-ids (set (:source-ordered-ids source))
     :proof-count (count checked)
     :witness-projection (:witness-projection (last obligations))
     :kernel-checked-proofs checked
     :scope :declared-initial-predicate-and-tracked-field-witness
     :actual-installed-initialization :not-established
     :certificate :not-issued :admission :not-admitted :authority :none}))

(defn- check-composed! [catalog]
  (let [initial (check-initialization! catalog)
        preservation (pilot! preservation-ns 'check-catalog!
                             (dissoc catalog :initial-condition))]
    (when-not (= (:preservation-fingerprint initial)
                 (:catalog-fingerprint preservation))
      (reject! :mismatched-preservation-source
               {:initial (:preservation-fingerprint initial)
                :preservation (:catalog-fingerprint preservation)}))
    {:kind :conditional-mixed-abstract-safety-ingredients
     :source-fingerprint (:source-fingerprint initial)
     :preservation-fingerprint (:preservation-fingerprint initial)
     :initialization-proofs (:proof-count initial)
     :preservation-proofs (:proof-count preservation)
     :witness-projection (:witness-projection initial)
     :open-assumptions open-assumptions
     :inductive-composition :not-yet-proved-for-generic-catalogs
     :actual-installed-initialization :not-established
     :certificate :not-issued :admission :not-admitted :authority :none}))

(defn- initialized [base]
  (let [leaves (mapv :expression (vals (:invariants base)))]
    (assoc base :initial-condition
           {:readers (set (mapcat :readers (vals (:invariants base))))
            :expression (into [:and] leaves)})))

(deftest mixed-catalog-has-complete-initialization-and-witness-proofs
  (let [base (pilot! preservation-ns 'mixed-catalog)
        catalog (initialized base)
        report (check-composed! catalog)
        initial (check-initialization! catalog)]
    (is (= 4 (:initialization-proofs report)))
    (is (= 12 (:preservation-proofs report)))
    (is (= #{:queue/lower :queue/upper :queue/status-domain}
           (:invariant-ids initial)))
    (is (= {:queue/count 0 :queue/status :busy} (:witness-projection report)))
    (is (= :not-established (:actual-installed-initialization report)))
    (is (= :not-issued (:certificate report)))
    (is (= :none (:authority report)))
    (is (every? #(kernel/verifies? (ansatz/env) (:goal %) (:proof %))
                (:kernel-checked-proofs initial)))))

(deftest original-reservation-uses-the-generic-initialization-path
  (let [result (check-composed! (example/reservation-catalog))]
    (is (= 4 (:initialization-proofs result)))
    (is (= 6 (:preservation-proofs result)))
    (is (= {:slot/active-claims 0 :slot/status :available}
           (:witness-projection result)))
    (is (= :not-established (:actual-installed-initialization result)))))

(deftest signed-bounds-and-reordered-source-remain-provable
  (let [base (pilot! preservation-ns 'example-catalog
                     :queue/temperature -20 30 [-5 30])
        source (initialized base)
        rearranged (update-in source [:initial-condition :expression]
                              (fn [e] (into [:and] (reverse (rest e)))))
        a (check-initialization! source)
        b (check-initialization! rearranged)]
    (is (= 3 (:proof-count a)))
    (is (= {:queue/temperature -20} (:witness-projection a)))
    (is (= (:invariant-ids a) (:invariant-ids b)))
    (is (not= (:source-fingerprint a) (:source-fingerprint b)))))

(deftest singleton-enum-initialization-is-a-real-checked-subset
  (let [base (pilot! preservation-ns 'mixed-catalog)
        singleton-rule [:in [:read :queue/status] [:literal #{:busy}]]
        source (-> (initialized base)
                   (assoc-in [:invariants :queue/status-domain :expression]
                             singleton-rule)
                   (assoc-in [:initial-condition :expression 3]
                             singleton-rule)
                   (assoc-in [:operations 0 :construction :replacements
                              :queue/status] [:literal :busy])
                   (assoc-in [:operations 1 :construction :replacements
                              :queue/status] [:literal :busy])
                   (assoc-in [:operations 2 :construction :replacements
                              :queue/status] [:literal :busy]))
        result (check-composed! source)]
    (is (= 4 (:initialization-proofs result)))
    (is (= 12 (:preservation-proofs result)))
    (is (= :busy (get (:witness-projection result) :queue/status)))
    (is (= :not-issued (:certificate result)))))

(deftest missing-duplicated-or-weakened-initial-predicates-fail-closed
  (let [source (initialized (pilot! preservation-ns 'mixed-catalog))
        reason (fn [bad]
                 (try (source-initialization-obligations! bad) nil
                      (catch clojure.lang.ExceptionInfo e
                        (:reason (ex-data e)))))
        leaves (get-in source [:initial-condition :expression])]
    (doseq [bad [(update-in source [:initial-condition :expression] pop)
                 (assoc-in source [:initial-condition :expression 2]
                           (nth leaves 1))
                 (assoc-in source [:initial-condition :expression 0] :or)
                 (assoc-in source [:initial-condition :readers]
                           #{:queue/status})
                 (dissoc source :initial-condition)]]
      (is (= :unsupported-initial-condition (reason bad))))
    (is (not= (:source-fingerprint (check-initialization! source))
              (:source-fingerprint
               (check-initialization!
                (assoc-in source [:initial-condition :expression]
                          (into [:and] (reverse (rest leaves))))))))))

(deftest contradictory-integer-and-enum-constraints-are-not-vacuously-accepted
  (let [source (initialized (pilot! preservation-ns 'mixed-catalog))
        lower' [:>= [:read :queue/count] [:literal 5]]
        updated (-> source
                    (assoc-in [:invariants :queue/lower :expression] lower')
                    (assoc-in [:initial-condition :expression 1] lower'))
        reason (fn [bad]
                 (try (source-initialization-obligations! bad) nil
                      (catch clojure.lang.ExceptionInfo ex
                        (:reason (ex-data ex)))))
        disjoint-invariant {:readers #{:queue/status}
                            :expression [:in [:read :queue/status]
                                         [:literal #{:ready}]]}
        disjoint (-> source
                     (assoc-in [:invariants :queue/second-status] disjoint-invariant)
                     (assoc-in [:invariants :queue/status-domain :expression 2 1]
                               #{:busy :closed})
                     (update-in [:initial-condition :expression]
                                (fn [e] (into (vec e) [(:expression disjoint-invariant)])))
                     (assoc-in [:initial-condition :expression 3]
                               [:in [:read :queue/status]
                                [:literal #{:busy :closed}]]))]
    (is (= :unsatisfiable-initial-integer-bounds (reason updated)))
    ;; In this actual catalog, changed status constraints also invalidate a
    ;; literal write, so preservation rejects BEFORE the witness search.
    (is (= :enum-write-outside-invariant (reason disjoint)))
    (is (= :unsatisfiable-initial-enum-intersection
           (try (tracked-witness!
                 {:field-names [:queue/status]
                  :invariants
                  {:queue/a {:kind :finite-enum-membership
                             :field :queue/status :members #{:busy}}
                   :queue/b {:kind :finite-enum-membership
                             :field :queue/status :members #{:ready}}}})
                nil
                (catch clojure.lang.ExceptionInfo ex
                  (:reason (ex-data ex))))))))

(defn -main [& _]
  (binding [ansatz/*verbose* false] (ansatz/load-init!))
  (let [{:keys [fail error]} (run-tests
                             'gesso.assurance.ansatz-mixed-initialization-pilot)]
    (when (pos? (+ fail error)) (System/exit 1))))

(ns gesso.assurance.proof.propositional-normalization-audit-test
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.assurance.proof.proposition :as proposition]
            [gesso.assurance.proof.propositional-normalization-audit :as audit]))

(def subject {:kind :operation :id :example/advance
              :interface {:operation :example/advance}})
(def a [:atom {:readers #{:request/score}
               :expression [:> [:read :request/score] [:literal 7]]}])
(def b [:atom {:readers #{:request/status}
               :expression [:in [:read :request/status] [:literal #{:status/active :status/new}]]}])
(def types {:request/score {:type :integer}
            :request/status {:type :enum :name :status/kind
                             :values #{:status/active :status/new}}})
(defn source [p v] {:subject subject :variables v :proposition p})
(defn reason [f] (try (f) nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))

(deftest boolean-connectives-and-closed-atoms
  (doseq [[p vars expected] [[[:true] {} 1]
                             [[:false] {} 1]
                             [[:implies a a] {:request/score (:request/score types)} 2]
                             [[:not [:and a b]] types 4]
                             [[:or [:false] [:and a b] [:not a]] types 4]]]
    (let [r (audit/audit-proposition! (source p vars))]
      (is (= :preserved-for-all-opaque-valuations (:status r)))
      (is (= expected (:valuations-checked r)))
      (is (= :exact-match (:connective-shape r)))
      (is (= :not-established (:typed-atomic-denotational-equivalence r)))
      (is (= :not-issued (:certificate r))))))

(deftest default-type-flags-and-integer-canonicalization
  (let [p [:atom {:readers #{:request/score}
                  :expression [:>= [:read :request/score] [:literal 7N]]}]
        r (audit/audit-proposition! (source [:and p a]
                                           {:request/score {:type :integer :nullable? false}}))]
    (is (= :preserved-for-all-opaque-valuations (:status r)))
    (is (= 4 (:valuations-checked r)))))

(deftest no-certificate-or-typed-proof
  (let [r (audit/audit-proposition! (source [:implies a a]
                                           {:request/score {:type :integer}}))]
    (is (= :none (:authority r)))
    (is (= :not-admitted (:admission r)))
    (is (= :not-established (:universal-normalizer-implementation-soundness r)))
    (is (= :not-established (:kernel-derivation-soundness r)))))

(deftest deliberate-normalizer-connective-fault-rejected
  (let [orig proposition/describe-proposition!]
    (with-redefs [proposition/describe-proposition!
                  (fn [v] (update-in (orig v) [:semantics :proposition]
                                     (fn [form] (assoc form 0 :or))))]
      (is (= :connective-structure-mismatch
             (reason #(audit/audit-proposition! (source [:and a b] types))))))))

(deftest deliberate-normalizer-atomic-fault-rejected
  (let [orig proposition/describe-proposition!]
    (with-redefs [proposition/describe-proposition!
                  (fn [v] (update-in (orig v) [:semantics :proposition 1 1 :expression]
                                     (fn [_] [:literal false])))]
      (is (= :atomic-canonicalization-mismatch
             (reason #(audit/audit-proposition! (source [:not a]
                                                       {:request/score (:request/score types)}))))))))

(deftest original-source-validation-is-retained
  (is (keyword? (reason #(audit/audit-proposition!
                          (source [:atom {:readers #{:request/score}
                                          :expression [:now [:read :request/score]]}]
                                  {:request/score (:request/score types)})))))
  (is (keyword? (reason #(audit/audit-proposition!
                          (source [:and a b] {:request/score (:request/score types)}))))))

(deftest atom-limit-stays-open
  (let [ids (mapv #(keyword "source" (str %)) (range 9))
        atoms (mapv (fn [id] [:atom {:readers #{id}
                                     :expression [:present [:read id]]}]) ids)
        values (into {} (map (fn [id] [id {:type :boolean}])) ids)
        r (audit/audit-proposition! (source (into [:and] atoms) values))]
    (is (= :not-checked (:status r)))
    (is (= :atom-budget (:reason r)))
    (is (= 0 (:valuations-checked r)))))

(deftest zero-atoms-and-repeated-atoms
  (let [r (audit/audit-proposition! (source [:and [:true] [:not [:false]]] {}))]
    (is (= :preserved-for-all-opaque-valuations (:status r)))
    (is (= 1 (:valuations-checked r))))
  (let [r (audit/audit-proposition! (source [:and a [:not a] a]
                                           {:request/score (:request/score types)}))]
    (is (= 3 (:atomic-occurrences r)))
    (is (= 1 (:distinct-source-atoms r)))
    (is (= 2 (:valuations-checked r)))))

(deftest altered-type-descriptor-and-version-rejected
  (let [orig proposition/describe-proposition!
        src (source a {:request/score (:request/score types)})]
    (with-redefs [proposition/describe-proposition!
                  (fn [v] (assoc-in (orig v)
                                    [:semantics :proposition 1 :reader-types :request/score :type]
                                    :string))]
      (is (= :atomic-canonicalization-mismatch
             (reason #(audit/audit-proposition! src)))))
    (with-redefs [proposition/describe-proposition!
                  (fn [v] (assoc-in (orig v)
                                    [:semantics :proposition 1 :syntax-version]
                                    999))]
      (is (= :atomic-canonicalization-mismatch
             (reason #(audit/audit-proposition! src)))))))

(deftest metadata-bearing-canonical-atom-is-rejected
  (let [orig proposition/describe-proposition!
        src (source a {:request/score (:request/score types)})]
    (with-redefs [proposition/describe-proposition!
                  (fn [v] (update-in (orig v)
                                     [:semantics :proposition 1 :expression]
                                     (fn [x] (with-meta x {:forged true}))))]
      (is (= :metadata-not-allowed
             (reason #(audit/audit-proposition! src)))))))


;; v1338 regression expansion. This is a permanent verification suite for the
;; *source-relative* v1337 audit, not an admission test for proof certificates.
(defn- checked
  ([proposition variables]
   (audit/audit-proposition! (source proposition variables)))
  ([proposition]
   (checked proposition {:request/score (:request/score types)})))

(defn- faulty-normalization
  "Replace only the return value of the production normalizer.  The source
   declaration remains the original and must still be checked independently."
  [declaration fault]
  (let [original proposition/describe-proposition!]
    (with-redefs [proposition/describe-proposition!
                  (fn [v] (fault (original v)))]
      (audit/audit-proposition! declaration))))

(defn- canonical-fault-reason [declaration fault]
  (reason #(faulty-normalization declaration fault)))

(defn- atom-for [id]
  [:atom {:readers #{id} :expression [:present [:read id]]}])

(deftest v1338-deep-connective-tree
  (let [p [:implies
           [:and [:or a [:not b]]
            [:implies [:false] [:or a [:true]]]]
           [:or [:not [:and a b]]
            [:and [:true] [:implies a b]]]]
        r (checked p types)]
    (is (= :preserved-for-all-opaque-valuations (:status r)))
    (is (= :exact-match (:connective-shape r)))
    (is (= 4 (:valuations-checked r)))
    (is (= 2 (:distinct-source-atoms r)))
    (is (= 7 (:atomic-occurrences r)))))

(deftest v1338-constants-and-negated-constants
  (doseq [p [[:true] [:false] [:not [:true]] [:not [:false]]
             [:implies [:false] [:false]]
             [:and [:true] [:not [:false]]]
             [:or [:false] [:and [:true] [:false]]]]]
    (let [r (checked p {})]
      (is (= :preserved-for-all-opaque-valuations (:status r)))
      (is (= 0 (:distinct-source-atoms r)))
      (is (= 1 (:valuations-checked r)))
      (is (= :none (:authority r))))))

(deftest v1338-repeated-atom-identity-is-not-inflated
  (let [p [:or [:and a a [:not a]]
           [:implies [:not a] [:or a a]]]
        r (checked p)]
    (is (= :preserved-for-all-opaque-valuations (:status r)))
    (is (= 6 (:atomic-occurrences r)))
    (is (= 1 (:distinct-source-atoms r)))
    (is (= 2 (:valuations-checked r)))))

(deftest v1338-distinct-source-atoms-stay-distinct
  (let [greater [:atom {:readers #{:request/score}
                        :expression [:> [:read :request/score] [:literal 7]]}]
        greater-equal [:atom {:readers #{:request/score}
                              :expression [:>= [:read :request/score] [:literal 7]]}]
        r (checked [:implies greater greater-equal])]
    ;; Boolean shape checking does NOT conclude that >7 implies >=7.
    (is (= :preserved-for-all-opaque-valuations (:status r)))
    (is (= 2 (:distinct-source-atoms r)))
    (is (= 4 (:valuations-checked r)))
    (is (= :not-established (:typed-atomic-denotational-equivalence r)))))

(deftest v1338-atom-order-does-not-affect-valuation-count
  (doseq [p [[:and a b] [:and b a] [:or a b] [:or b a]
             [:implies a b] [:implies b a]]]
    (let [r (checked p types)]
      (is (= :preserved-for-all-opaque-valuations (:status r)))
      (is (= 4 (:valuations-checked r)))
      (is (= 2 (:distinct-source-atoms r))))))

(deftest v1338-canonical-result-is-deterministic
  (let [s (source [:implies [:or a b] [:not [:and a b]]] types)]
    (is (= (audit/audit-proposition! s) (audit/audit-proposition! s)))
    (is (= (:source-fingerprint (audit/audit-proposition! s))
           (:fingerprint (proposition/describe-proposition! s))))))

(deftest v1338-structural-source-change-alters-fingerprint
  (let [left (checked [:and a b] types)
        right (checked [:or a b] types)]
    (is (not= (:source-fingerprint left) (:source-fingerprint right)))
    (is (= :preserved-for-all-opaque-valuations (:status left)))
    (is (= :preserved-for-all-opaque-valuations (:status right)))))

(deftest v1338-semantic-scope-never-promotes-certificate
  (let [r (checked [:implies a a])]
    (is (= :preserved-for-all-opaque-valuations (:status r)))
    (is (= :opaque-propositional-connectives (:semantic-scope r)))
    (is (= :source-to-normalized-atomic-valuation (:correspondence r)))
    (is (= :not-issued (:certificate r)))
    (is (= :not-admitted (:admission r)))
    (is (= :none (:authority r)))
    (is (= :not-established (:universal-normalizer-implementation-soundness r)))
    (is (= :not-established (:kernel-derivation-soundness r)))
    (is (= :not-established (:typed-atomic-denotational-equivalence r)))
    (is (= :not-performed (:assumption-discharge r)))))

(deftest v1338-dropped-or-appended-conjuncts-rejected
  (let [s (source [:and a b] types)]
    (is (= :connective-structure-mismatch
           (canonical-fault-reason
            s (fn [r] (update-in r [:semantics :proposition] pop)))))
    (is (= :connective-structure-mismatch
           (canonical-fault-reason
            s (fn [r] (update-in r [:semantics :proposition]
                                 conj [:true])))))))

(deftest v1338-swapped-branches-and-altered-operator-rejected
  (let [s (source [:implies a b] types)]
    (is (= :atomic-canonicalization-mismatch
           (canonical-fault-reason
            s (fn [r] (update-in r [:semantics :proposition]
                                 (fn [p] [:implies (nth p 2) (nth p 1)]))))))
    (is (= :connective-structure-mismatch
           (canonical-fault-reason
            s (fn [r] (assoc-in r [:semantics :proposition 0] :or)))))))

(deftest v1338-duplicated-atom-and-forged-constant-rejected
  (let [s (source [:and a b] types)]
    (is (= :atomic-canonicalization-mismatch
           (canonical-fault-reason
            s (fn [r] (assoc-in r [:semantics :proposition 2]
                                (get-in r [:semantics :proposition 1]))))))
    (is (= :connective-structure-mismatch
           (canonical-fault-reason
            s (fn [r] (assoc-in r [:semantics :proposition 2] [:true])))))))

(deftest v1338-added-and-missing-canonical-atom-keys-rejected
  (let [s (source a {:request/score (:request/score types)})]
    (is (= :atomic-canonicalization-mismatch
           (canonical-fault-reason
            s (fn [r] (assoc-in r [:semantics :proposition 1 :forged-proof] true)))))
    (is (= :atomic-canonicalization-mismatch
           (canonical-fault-reason
            s (fn [r] (update-in r [:semantics :proposition 1]
                                 dissoc :evaluator-version)))))))

(deftest v1338-altered-reader-footprint-rejected
  (let [s (source a {:request/score (:request/score types)})]
    (is (= :atomic-canonicalization-mismatch
           (canonical-fault-reason
            s (fn [r] (assoc-in r [:semantics :proposition 1 :readers]
                                #{:request/foreign})))))))

(deftest v1338-altered-literal-rejected
  (let [s (source a {:request/score (:request/score types)})]
    (is (= :atomic-canonicalization-mismatch
           (canonical-fault-reason
            s (fn [r] (assoc-in r [:semantics :proposition 1 :expression 2 1]
                                8N)))))))

(deftest v1338-altered-normalizer-type-flags-rejected
  (let [s (source a {:request/score (:request/score types)})]
    (doseq [key [:optional? :nullable?]]
      (is (= :atomic-canonicalization-mismatch
             (canonical-fault-reason
              s (fn [r] (update-in r [:semantics :proposition 1 :reader-types :request/score key]
                                   not))))))))

(deftest v1338-canonical-root-metadata-rejected
  (let [s (source [:not a] {:request/score (:request/score types)})]
    (is (= :metadata-not-allowed
           (canonical-fault-reason
            s (fn [r] (update-in r [:semantics :proposition]
                                 #(with-meta % {:do-not-trust true}))))))))

(deftest v1338-nested-reader-types-metadata-rejected
  (let [s (source a {:request/score (:request/score types)})]
    (is (= :metadata-not-allowed
           (canonical-fault-reason
            s (fn [r] (update-in r [:semantics :proposition 1 :reader-types :request/score]
                                 #(with-meta % {:hidden true}))))))))

(deftest v1338-nested-readers-set-metadata-rejected
  (let [s (source a {:request/score (:request/score types)})]
    (is (= :metadata-not-allowed
           (canonical-fault-reason
            s (fn [r] (update-in r [:semantics :proposition 1 :readers]
                                 #(with-meta % {:hidden true}))))))))

(deftest v1338-source-metadata-rejected
  (let [s (with-meta (source a {:request/score (:request/score types)})
            {:source-rewrite true})]
    (is (= :metadata-not-allowed (reason #(audit/audit-proposition! s))))))

(deftest v1338-fabricated-approval-in-source-rejected
  (let [s (assoc (source a {:request/score (:request/score types)})
                 :certificate :approved)]
    (is (= :wrong-declaration-keys (reason #(audit/audit-proposition! s))))))

(deftest v1338-unsupported-source-is-not-canonicalized-into-safety
  (is (= :unknown-connective
         (reason #(checked [:xor a a]))))
  (is (= :unused-variable
         (reason #(checked a types)))))

(deftest v1338-nine-atoms-are-not-exhaustively-checked
  (let [ids (mapv #(keyword "n" (str %)) (range 9))
        v (zipmap ids (repeat {:type :boolean}))
        p (into [:and] (map atom-for ids))
        r (checked p v)]
    (is (= :not-checked (:status r)))
    (is (= :atom-budget (:reason r)))
    (is (= 0 (:valuations-checked r)))
    (is (= :not-issued (:certificate r)))
    (is (= :not-admitted (:admission r)))))

(deftest v1338-work-budget-does-not-turn-into-a-proof
  ;; 8 atoms are within the atom budget, but sufficiently many occurrences
  ;; make the 2^8-valuation cross-check exceed the independent work budget.
  (let [ids (mapv #(keyword "w" (str %)) (range 8))
        vars (zipmap ids (repeat {:type :boolean}))
        atoms (mapv atom-for ids)
        branch (into [:and] (take 112 (cycle atoms)))
        r (checked [:and branch branch] vars)]
    (is (= :not-checked (:status r)))
    (is (= :evaluation-work-budget (:reason r)))
    (is (= 8 (:distinct-source-atoms r)))
    (is (= 0 (:valuations-checked r)))
    (is (= :not-issued (:certificate r)))))

(deftest v1338-canonical-atom-set-normalization
  (let [atom [:atom {:readers #{:request/status}
                     :expression [:in [:read :request/status]
                                  [:literal #{:status/active :status/new}]]}]
        r (checked [:and atom [:not atom]]
                   {:request/status (:request/status types)})]
    (is (= :preserved-for-all-opaque-valuations (:status r)))
    (is (= 1 (:distinct-source-atoms r)))
    (is (= 2 (:valuations-checked r)))))

(deftest v1338-corrupted-inner-negation-rejected
  (let [s (source [:or [:not a] b] types)]
    (is (= :connective-structure-mismatch
           (canonical-fault-reason
            s (fn [r] (assoc-in r [:semantics :proposition 1 0] :and)))))))

(deftest v1338-spoofed-audit-claims-are-not-source-declarations
  (let [s (merge (source [:true] {})
                 {:authority :proof :status :preserved-for-all-opaque-valuations})]
    (is (= :wrong-declaration-keys
           (reason #(audit/audit-proposition! s))))))

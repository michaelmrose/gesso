(ns gesso.assurance.reference-configured-recovery-inventory-test
  (:require [clojure.set :as set]
            [clojure.test :refer [deftest is]]
            [gesso.assurance.assumption :as assumption]
            [gesso.assurance.reference-configured-recovery-inventory :as configured]
            [gesso.assurance.reference-recovery-inventory :as historical]
            [gesso.assurance.reference-storage-identity :as storage]))

(defn- descriptions [family n]
  (mapv (fn [i]
          (let [id (keyword (name family) (str "premise-" i))]
            (assumption/describe-assumption
             {:id id :subject {:kind :runtime :id :fixture/subject :interface {:family family}}
              :scope {:kind :runtime :id :fixture/scope} :proposition [id]
              :parameters {} :semantic-context {} :origin :environment
              :implementation-boundary :fixture/boundary
              :dependencies (if (zero? i) #{} #{(keyword (name family) (str "premise-" (dec i)))})})))
        (range n)))

(defn- family [name n adopted?]
  (let [ds (descriptions name n) fps (set (map :premise-fingerprint ds))]
    (merge {:obligations ds :premises (into {} (map (juxt #(get-in % [:assumption :id]) identity)) ds)
            :open-premise-fingerprints fps :proof-discharge :not-performed}
           (if adopted?
             {:trusted-assumptions fps :deployment-adoption :configured-local-host :premise-acceptance :explicit-declaration-only}
             {:premise-acceptance :not-performed}))))

(defn- fixture []
  ;; Structural fixture only. The installed-host regression exercises the
  ;; actual issued, source-relative declarations and observations.
  (let [cut {:payload-binding-profile storage/cut-profile :record-offsets [0 1 2]
             :transaction-ids [1 2] :processed-message-id 2 :payload-fingerprint (apply str (repeat 64 "a"))}
        scope {:comparison-id :fixture/comparison :identity-policy :fixture/policy
               :origin-boundary :fixture/origin :acquisition-boundary :fixture/acquisition
               :identity-boundary :fixture/identity :publication-source-id "source"
               :origin-observation-ref "origin" :retained-acquisition-ref "acquired"
               :relation :authorized-historical-lineage :historical-cut cut}
        stored (assoc (storage/describe-deployment-acceptance scope
                              {:deployment-id :fixture/deployment :identity-policy :fixture/policy
                               :acceptance :trust-open-premises
                               :premises (mapv :assumption (:obligations (storage/describe-obligations scope)))})
                      :status :conditional :judgment-scope scope :proof-discharge :not-performed)
        register {:fixture/operation #{:fixture/original}}
        abstract {:fingerprint "abstract" :declared-operation-ids (set (keys register))
                  :classification {:open {:assumptions #{} :assumption-classes {} :certificate-premises #{}}
                                   :certificate {:open-premises #{}}
                                   :original-obligations {:status :all-original-obligations-remain-open :by-operation register}}}
        original (historical/describe-inventory abstract stored (family :command 1 false)
                                                (family :worker 1 false) (family :physical 1 false))
        writer (family :writer-isolation 5 true) provider (family :provider-trust 8 true)
        general #{:writer-condition :provider-condition :other-condition}
        history-conditions #{:history-condition}
        binding {:status :conditional-original-clm-r2-binding :abstract-report-fingerprint "abstract"
                 :original-open-obligation-ids-by-operation register :remaining-conditions general}]
    [{:status :conditional-configured-implementation-argument :original-clm-r2-binding binding
      :remaining-conditions general :writer-isolation writer :provider-trust provider
      :trusted-assumptions-by-contract {:writer-isolation (:trusted-assumptions writer) :provider-trust (:trusted-assumptions provider)}
      :condition-links-by-contract {:writer-isolation #{:writer-condition} :provider-trust {:provider-condition #{:provider-trust/premise-0}}}
      :condition-link-semantics :related-not-entailing :proof-discharge :not-performed}
     {:status :conditional-physical-source-envelope-recovery :general-argument {:general-conditions general}
      :remaining-conditions (conj general :physical-condition) :proof-discharge :not-performed}
     {:status :conditional-configured-selected-writer-history :historical-cut cut
      :remaining-conditions history-conditions :proof-discharge :not-performed}
     {:status :conditional :historical-judgment stored :original-clm-r2-binding binding :historical-cut cut
      :obligation-inventory original :general-remaining-conditions general
      :selected-remaining-conditions history-conditions :proof-discharge :not-performed}
     (assoc stored :deployment-adoption :configured-local-host)]))

(deftest preserves-distinct-open-families-and-conditions
  (let [[c p h r _ :as args] (fixture)
        result (apply configured/compose! args)
        all (:all-described-open-premise-fingerprints result)
        adopted (:deployment-declared-premise-fingerprints result)
        unaccepted (:unaccepted-premise-fingerprints result)
        index (:premises-by-fingerprint result)
        order (zipmap (:dependency-order result) (range))]
    (is (= :conditional-configured-obligation-inventory (:status result)))
    (is (= 19 (count all) (count order)))
    (is (= 16 (count adopted)))
    (is (= 3 (count unaccepted)))
    (is (empty? (set/intersection adopted unaccepted)))
    (is (= (:obligation-inventory r) (:original-historical-inventory result)))
    (is (= (set/union (:remaining-conditions p) (:remaining-conditions h)) (:remaining-conditions result)))
    (is (= (:remaining-conditions c) (:remaining-general-conditions result)))
    (is (= :not-derived (get-in result [:storage-relevance :general-condition-links])))
    (is (every? (fn [[fp row]] (every? #(< (order %) (order fp)) (:dependency-fingerprints row))) index))
    (is (every? #(= :open (:discharge-state %)) (vals index)))
    (is (= :not-performed (:proof-discharge result)))))

(deftest mismatched-premises-and-conditions-reject
  (let [[c p h r a :as args] (fixture)]
    (doseq [bad [(assoc args 0 (assoc-in c [:writer-isolation :obligations 0 :discharge-state] :discharged))
                 (assoc args 0 (assoc-in c [:provider-trust :premises :provider-trust/premise-0 :premise-fingerprint] "forged"))
                 (assoc args 0 (assoc-in c [:condition-links-by-contract :provider-trust :provider-condition] #{:foreign/premise}))
                 (assoc args 0 (assoc-in c [:condition-links-by-contract :writer-isolation] #{:unrelated}))
                 (assoc args 1 (update p :remaining-conditions disj :writer-condition))
                 (assoc args 2 (assoc h :remaining-conditions #{}))
                 (assoc args 3 (assoc r :historical-cut {}))
                 (assoc args 4 (assoc a :trusted-assumptions #{}))
                 (assoc args 3 (assoc-in r [:obligation-inventory :premises-by-fingerprint
                                          (first (get-in r [:obligation-inventory :premise-families :command]))
                                          :dependency-fingerprints] #{"missing"}))]]
      (is (thrown? clojure.lang.ExceptionInfo (apply configured/compose! bad))))
    (with-redefs [assumption/describe-assumption (constantly {})]
      (is (thrown? clojure.lang.ExceptionInfo (apply configured/compose! args))))))

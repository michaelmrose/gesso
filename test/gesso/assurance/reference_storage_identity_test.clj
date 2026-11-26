(ns gesso.assurance.reference-storage-identity-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is]]
            [gesso.assurance.assumption :as assumption]
            [gesso.assurance.reference-storage-identity :as storage]))

(def ^:private scope
  {:comparison-id :fixture/selected-recovery
   :publication-source-id "claim"
   :origin-observation-ref "original-observation"
   :retained-acquisition-ref "reopened-history"
   :relation :same-store-incarnation
   :identity-policy :fixture/identity-policy-v1
   :origin-boundary :fixture/original-adapter
   :acquisition-boundary :fixture/retained-reader
   :identity-boundary :fixture/store-controller})

(defn- describe [s] (storage/describe-obligations s))
(defn- fingerprints [r] (mapv :premise-fingerprint (:obligations r)))
(defn- reason [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))

(deftest descriptions-are-open-and-do-not-authenticate-identities
  (let [r (describe scope)
        [origin acquisition relation] (:obligations r)]
    (is (= storage/profile (:profile r)))
    (is (= :same-store-incarnation (:relation r)))
    (is (= [:reference-storage/origin-binding :reference-storage/acquisition-binding
            :reference-storage/same-incarnation]
           (mapv #(get-in % [:assumption :id]) (:obligations r))))
    (is (= 3 (count (distinct (fingerprints r)))))
    (is (= 1 (count (distinct (map :subject-fingerprint (:obligations r))))))
    (is (= #{} (get-in origin [:assumption :dependencies]) (get-in acquisition [:assumption :dependencies])))
    (is (= #{:reference-storage/origin-binding :reference-storage/acquisition-binding}
           (get-in relation [:assumption :dependencies])))
    (is (= [:fixture/original-adapter :fixture/retained-reader :fixture/store-controller]
           (mapv #(get-in % [:assumption :implementation-boundary]) (:obligations r))))
    (is (= r (describe (edn/read-string (pr-str scope)))))
    (is (= :not-established (:observation-storage-identity r) (:authorized-historical-lineage r)))
    (is (= :none (:authority r)))
    (is (= :not-issued (:certificate r)))
    (doseq [d (:obligations r)]
      (is (= :open (:discharge-state d)))
      (is (= :not-performed (:checking d)))
      (is (= :not-admitted (:admission d)))
      (let [a (:assumption d) m (assumption/match-premise! a (edn/read-string (pr-str a)))]
        (is (= :structural-only (:match m)))
        (is (= :open (:discharge-state m)))
        (is (= :not-admitted (:admission m)))))))

(deftest distinct-relations-and-scope-changes-do-not-share-premises
  (let [r (describe scope)
        lineage (describe (assoc scope :relation :authorized-historical-lineage))]
    (is (= :authorized-historical-lineage (:relation lineage)))
    (is (= :reference-storage/authorized-lineage (get-in lineage [:obligations 2 :assumption :id])))
    (is (= :not-established (:authorized-historical-lineage lineage) (:observation-storage-identity lineage)))
    (doseq [[k v] [[:comparison-id :fixture/another-comparison]
                   [:publication-source-id "different-source"]
                   [:origin-observation-ref "substituted-observation"]
                   [:retained-acquisition-ref "copied-store-acquisition"]
                   [:relation :authorized-historical-lineage]
                   [:identity-policy :fixture/identity-policy-v2]
                   [:origin-boundary :fixture/other-origin]
                   [:acquisition-boundary :fixture/other-reader]
                   [:identity-boundary :fixture/other-controller]]]
      (let [changed (describe (assoc scope k v))]
        (is (every? true? (map not= (fingerprints r) (fingerprints changed))))
        (is (= :none (:authority changed)))
        (is (= :not-established (:observation-storage-identity changed)))
        (is (= (if (= k :comparison-id) :scope-mismatch :subject-mismatch)
               (reason #(assumption/match-premise! (get-in r [:obligations 0 :assumption])
                                                  (get-in changed [:obligations 0 :assumption]))))
            (str k))))))

(deftest rejects-purported-evidence-and-unsupported-relations
  (doseq [bad [(assoc scope :certificate "signed") (assoc scope :matched true)
               (assoc scope :fingerprint "sha256:copy") (assoc scope :discharge-state :discharged)
               (assoc scope :authenticated? true) (assoc scope :path "/var/lib/xtdb")
               (dissoc scope :identity-boundary) (with-meta scope {:trusted true}) nil]]
    (is (= :invalid-obligation-scope (reason #(describe bad)))))
  (doseq [v [:content-equality :same-path :same-uuid :same-hash :authorized-writer nil]]
    (is (= :unsupported-storage-identity-relation (reason #(describe (assoc scope :relation v))))))
  (doseq [[k v] [[:comparison-id :unqualified] [:identity-policy "policy"]
                 [:origin-boundary (Object.)] [:acquisition-boundary nil] [:identity-boundary :local]]]
    (is (= :invalid-boundary-or-policy-name (reason #(describe (assoc scope k v))))))
  (doseq [[k v] [[:publication-source-id " "] [:origin-observation-ref nil]
                 [:retained-acquisition-ref (Object.)] [:retained-acquisition-ref (apply str (repeat 1025 "x"))]]]
    (is (= :invalid-observation-reference (reason #(describe (assoc scope k v)))))))

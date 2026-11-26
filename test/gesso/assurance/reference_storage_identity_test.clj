(ns gesso.assurance.reference-storage-identity-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is]]
            [gesso.assurance.assumption :as assumption]
            [gesso.assurance.reference-storage-identity :as storage]))

(def ^:private cut
  {:payload-binding-profile storage/cut-profile
   :record-offsets [0 51 101 201]
   :transaction-ids [0 101 201]
   :processed-message-id 201
   :payload-fingerprint (apply str (repeat 64 "a"))})

(def ^:private scope
  {:comparison-id :fixture/selected-recovery
   :publication-source-id "claim"
   :origin-observation-ref "original-observation"
   :retained-acquisition-ref "reopened-history"
   :historical-cut cut
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
    (is (every? #(= cut (get-in % [:assumption :subject :interface :historical-cut]))
                (:obligations r)))
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

(deftest historical-cut-is-part-of-every-premise
  (let [r (describe scope)
        alternatives [(assoc cut :payload-fingerprint (apply str (repeat 64 "b")))
                      (assoc cut :transaction-ids [0 201])
                      (assoc cut :record-offsets [0 52 101 201])
                      (assoc cut :record-offsets [0 51 101 201 301]
                                 :transaction-ids [0 101 201 301]
                                 :processed-message-id 301)]]
    (doseq [changed-cut alternatives]
      (let [changed (describe (assoc scope :historical-cut changed-cut))]
        (is (every? true? (map not= (fingerprints r) (fingerprints changed))))
        (doseq [[a b] (map vector (:obligations r) (:obligations changed))]
          (is (= :subject-mismatch
                 (reason #(assumption/match-premise! (:assumption a) (:assumption b)))))
          (is (= :open (:discharge-state b))))
        (is (= :none (:authority changed)))
        (is (= :not-issued (:certificate changed)))))))

(deftest rejects-invalid-or-unqualified-cut-declarations
  (is (= :invalid-obligation-scope (reason #(describe (dissoc scope :historical-cut)))))
  (doseq [bad [nil (dissoc cut :payload-fingerprint)
               (assoc cut :authenticated? true)
               (with-meta cut {:trusted true})
               (assoc cut :payload-binding-profile :fixture/unknown)
               (assoc cut :record-offsets [51 101 201])
               (assoc cut :record-offsets [0 101 51 201])
               (assoc cut :record-offsets [0 51 101 101 201])
               (assoc cut :record-offsets (range 1025))
               (assoc cut :record-offsets (vec (range 1025)))
               (assoc cut :record-offsets [0 51 101 201 (inc (bigint Long/MAX_VALUE))])
               (assoc cut :record-offsets (with-meta [0 51 101 201] {:trusted true}))
               (assoc cut :transaction-ids [0 201 101])
               (assoc cut :transaction-ids [0 101 101 201])
               (assoc cut :transaction-ids [0 102 201])
               (assoc cut :record-offsets (vec (range 257))
                          :transaction-ids (vec (range 257)) :processed-message-id 256)
               (assoc cut :processed-message-id 101)
               (assoc cut :processed-message-id 202)
               (assoc cut :processed-message-id 201.0)
               (assoc cut :payload-fingerprint "sha256:copy")
               (assoc cut :payload-fingerprint (apply str (repeat 64 "A")))]]
    (is (= :invalid-historical-cut (reason #(describe (assoc scope :historical-cut bad))))))
  (let [empty-cut (assoc cut :record-offsets [] :transaction-ids [] :processed-message-id -1)
        r (describe (assoc scope :historical-cut empty-cut))]
    (is (= :none (:authority r)))
    (is (every? #(= :open (:discharge-state %)) (:obligations r)))
    (is (= :invalid-historical-cut
           (reason #(describe (assoc scope :historical-cut (assoc empty-cut :processed-message-id 0))))))))

(defn- premises [s] (mapv :assumption (:obligations (describe s))))

(deftest selected-premises-are-exact-and-remain-open
  (doseq [relation storage/relations]
    (let [s (assoc scope :relation relation)
          ps (premises s)
          r (storage/describe-selected-premises s ps)]
      (is (= storage/selection-profile (:selection-profile r)))
      (is (= :exact-structural-only (:selection r)))
      (is (= :complete-for-declared-premises (:dependency-closure r)))
      (is (= :not-performed (:premise-acceptance r)))
      (is (= (describe s) (dissoc r :selection-profile :selection :dependency-closure :premise-acceptance)))
      (is (= r (storage/describe-selected-premises s (vec (reverse ps)))))
      (is (= r (storage/describe-selected-premises s (edn/read-string (pr-str ps)))))
      (is (= :none (:authority r)))
      (is (= :not-issued (:certificate r)))
      (is (= :not-established (:observation-storage-identity r) (:authorized-historical-lineage r)))
      (doseq [p (:obligations r)]
        (is (= :open (:discharge-state p)))
        (is (= :not-performed (:checking p)))
        (is (= :not-admitted (:admission p)))))))

(deftest rejects-incomplete-or-substituted-premise-selections
  (let [[origin acquisition relation :as ps] (premises scope)
        select! #(storage/describe-selected-premises scope %)]
    (doseq [bad [nil (seq ps) (set ps) (describe scope) (with-meta ps {:accepted true})
                 (conj ps origin) (mapv :id ps) (assoc ps 0 (with-meta origin {:trusted true}))]]
      (is (= :invalid-premise-selection (reason #(select! bad)))))
    (doseq [bad [[] [origin] [origin acquisition] [relation] [origin relation]]]
      (is (= :incorrect-premise-set (reason #(select! bad)))))
    (is (= :duplicate-premise (reason #(select! [origin origin relation]))))
    (is (= :incorrect-premise-set
           (reason #(select! (premises (assoc scope :relation :authorized-historical-lineage))))))
    (doseq [changed [(assoc scope :identity-policy :fixture/other-policy)
                     (assoc scope :historical-cut (assoc cut :payload-fingerprint (apply str (repeat 64 "b"))))
                     (assoc scope :origin-observation-ref "different-origin")
                     (assoc scope :retained-acquisition-ref "different-acquisition")]]
      (is (= :subject-mismatch (reason #(select! (premises changed))))))
    (is (= :scope-mismatch (reason #(select! (premises (assoc scope :comparison-id :fixture/other))))))
    (doseq [modified [(assoc relation :dependencies #{})
                      (assoc relation :dependencies #{:fixture/unselected})
                      (assoc relation :dependencies #{(:id relation)})
                      (assoc relation :proposition [:fixture/always-true])
                      (assoc relation :implementation-boundary :fixture/other-controller)
                      (assoc relation :origin :other)]]
      (is (= :premise-mismatch (reason #(select! [origin acquisition modified])))))
    (doseq [modified [(assoc relation :discharge-state :discharged)
                      (assoc relation :certificate "signed")
                      (assumption/describe-assumption relation)]]
      (is (some? (reason #(select! [origin acquisition modified])))))))

(deftest explicit-deployment-trust-preserves-open-premises
  (doseq [relation storage/relations]
    (let [s (assoc scope :relation relation)
          ps (premises s)
          declaration {:deployment-id :fixture/deployment
                       :identity-policy (:identity-policy s)
                       :acceptance :trust-open-premises :premises ps}
          select! #(storage/describe-deployment-acceptance s %)
          r (select! declaration)]
      (is (= storage/acceptance-profile (:acceptance-profile r)))
      (is (= :explicit-declaration-only (:premise-acceptance r)))
      (is (= {:deployment-id :fixture/deployment
              :identity-policy (:identity-policy s)
              :authentication :not-established} (:deployment-acceptance r)))
      (is (= (set (fingerprints (describe s))) (:trusted-assumptions r)))
      (is (= (:obligations (describe s)) (:obligations r)))
      (is (= :complete-for-declared-premises (:dependency-closure r)))
      (is (= r (select! (update declaration :premises #(vec (reverse %))))))
      (is (= :none (:authority r)))
      (is (= :not-issued (:certificate r)))
      (is (= :not-established (:observation-storage-identity r) (:authorized-historical-lineage r)))
      (doseq [bad [nil true ps (dissoc declaration :acceptance)
                   (assoc declaration :acceptance true)
                   (assoc declaration :deployment-id :unqualified)
                   (assoc declaration :authenticated? true)
                   (assoc declaration :certificate "signed")
                   (with-meta declaration {:accepted true})]]
        (is (= :invalid-deployment-acceptance (reason #(select! bad)))))
      (is (= :acceptance-policy-mismatch
             (reason #(select! (assoc declaration :identity-policy :fixture/other)))))
      (is (= :incorrect-premise-set
             (reason #(select! (assoc declaration :premises (pop ps))))))
      (is (= :subject-mismatch
             (reason #(select! (assoc declaration :premises
                                      (premises (assoc s :retained-acquisition-ref "different-target")))))))
      (is (= :premise-mismatch
             (reason #(select! (update-in declaration [:premises 2] assoc :dependencies #{}))))))))

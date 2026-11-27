(ns gesso.assurance.reference-writer-isolation
  "Explicit, generation-scoped external writer isolation requirements.
   These diagnostics are not permissions or evidence of isolation."
  (:require [gesso.assurance.assumption :as assumption]
            [gesso.assurance.reference-admitted-step-argument :as admitted]
            [gesso.assurance.reference-installed-writer-classification :as installed]
            [gesso.live.consistency.xtdb :as xtdb]
            [gesso.live.outbox.writer-policy :as policy]))

(def profile :declared-external-writer-isolation-v1)
(deftype IsolationScope [])
(def ^:private scopes (java.util.WeakHashMap.))
(def ^:private original-classify installed/derive!)
(def ^:private original-general admitted/derive!)
(def ^:private original-token xtdb/current-submission-policy-token)
(def ^:private original-state xtdb/owned-submission-policy-state)
(def ^:private original-root policy/activated-policy-root)
(def ^:private original-describe assumption/describe-assumption)
(def ^:private original-match assumption/match-premise!)

(def ^:private base
  {:profile profile :writer-coverage :not-established
   :continuous-mediation :not-established :provider-correspondence :not-established
   :proof-discharge :not-performed :premise-acceptance :not-performed
   :authority :none :certificate :not-issued})

(def ^:private related-conditions
  #{:all-physical-writers-mediated-by-exact-installed-policy
    :continuous-exact-installed-policy-mediation
    :complete-admitted-writer-and-temporal-boundary-coverage
    :every-physical-step-instantiates-qualified-schema
    :complete-committed-aborted-and-temporal-boundary-classification})

(defn- reject! [reason]
  (throw (ex-info "Unqualified writer isolation declaration" {:reason reason})))

(defn- boundaries! []
  (when-not (and (identical? original-classify installed/derive!)
                 (identical? original-general admitted/derive!)
                 (identical? original-token xtdb/current-submission-policy-token)
                 (identical? original-state xtdb/owned-submission-policy-state)
                 (identical? original-root policy/activated-policy-root)
                 (identical? original-describe assumption/describe-assumption)
                 (identical? original-match assumption/match-premise!))
    (reject! :writer-isolation-boundary-replaced)))

(defn- named? [x] (and (keyword? x) (namespace x)))
(defn- plain-map? [x] (and (map? x) (not (record? x)) (nil? (meta x))))

(defn- validate-deployment! [deployment]
  (when-not (and (plain-map? deployment)
                 (= #{:deployment-id :database-id :isolation-boundary} (set (keys deployment)))
                 (every? named? (vals deployment)))
    (reject! :invalid-writer-isolation-deployment)))

(defn- current! [node admission token root]
  (when-not (and token root (identical? token (original-token node))
                 (= root (original-root node admission token))
                 (= :open (:phase (original-state node token))))
    (reject! :writer-isolation-generation-not-open)))

(defn- obligations [scope]
  (let [subject {:kind :runtime :id (:deployment-id scope) :interface scope}
        describe (fn [id proposition dependencies]
                   (original-describe
                    {:id id :subject subject :scope {:kind :runtime :id (:deployment-id scope)}
                     :proposition [proposition] :parameters {}
                     :semantic-context {:profile profile}
                     :dependencies dependencies :origin :environment
                     :implementation-boundary (:isolation-boundary scope)}))
        bind ::database-owner-binding
        local ::in-process-writer-exclusion
        external ::external-writer-exclusion
        storage ::storage-and-log-writer-exclusion]
    [(describe bind ::declared-owner-and-database-match-actual-protected-store #{})
     (describe local ::no-unmediated-raw-alternate-biff-or-diagnostic-writers #{bind})
     (describe external ::no-unmediated-external-provider-or-network-writers #{bind})
     (describe storage ::no-unmediated-storage-log-or-restore-mutation #{bind})
     (describe ::lifecycle-isolation ::exclusion-through-acquisition-activation-operation-drain-and-close
               #{bind local external storage})]))

(defn prepare!
  "Re-derive installed classification and issue a local scope for an explicit
   owner declaration. The owner labels are not authenticated and the node
   identity is not a durable database identity. Scope is limited to one
   generation, including its startup/cleanup interval as an OPEN premise.
   No caller-supplied classification report is accepted. Owner-only use."
  [node admission source declarations fingerprint cases deployment]
  (try
    (boundaries!)
    (validate-deployment! deployment)
    (let [token (original-token node)
          root (when token (original-root node admission token))]
      (current! node admission token root)
      (let [c (original-classify node admission source declarations fingerprint cases)
            g (original-general source declarations fingerprint cases)]
        (when-not (and (= :conditional-current-installed-writer-classification (:status c))
                       (= :open (:owned-policy-phase c))
                       (= fingerprint (:catalog-fingerprint c))
                       (= :conditional-general-admitted-step-derivation (:status g))
                       (= fingerprint (:catalog-fingerprint g))
                       (every? (:remaining-conditions g) related-conditions))
          (reject! :unqualified-isolation-classification))
        (let [scope (merge deployment
                           {:profile profile :scope-ref (str (java.util.UUID/randomUUID))
                            :catalog-fingerprint fingerprint
                            :induction-fingerprint (:induction-fingerprint c)
                            :protected-tables (:protected-tables c)
                            :activation-root (update (:activation-root c) :system-time str)
                            :interval :one-owned-generation-from-acquisition-through-close})
              required (obligations scope)
              handle (IsolationScope.)]
          (boundaries!)
          (current! node admission token root)
          (locking scopes
            (.put scopes handle {:node node :admission admission :token token :root root
                                 :scope scope}))
          (merge base {:status :open-isolation-requirements :scope scope :handle handle
                       :obligations required :related-condition-ids related-conditions
                       :condition-link-semantics :related-not-entailing
                       :local-lifecycle-evidence (:lifecycle-evidence c)}))))
    (catch clojure.lang.ExceptionInfo e
      (assoc base :status :not-established :reason (:reason (ex-data e))))))

(defn- select! [scope declaration]
  (let [required (obligations scope)
        offered (:premises declaration)
        ids (set (map #(get-in % [:assumption :id]) required))]
    (when-not (and (plain-map? declaration)
                   (= #{:scope :acceptance :premises} (set (keys declaration)))
                   (= scope (:scope declaration))
                   (= :trust-open-premises (:acceptance declaration))
                   (vector? offered) (nil? (meta offered))
                   (every? plain-map? offered)
                   (= (count ids) (count offered))
                   (= ids (set (map :id offered))))
      (reject! :incorrect-isolation-premise-selection))
    (let [by-id (into {} (map (juxt :id identity)) offered)]
      (doseq [{:keys [assumption]} required]
        (original-match assumption (get by-id (:id assumption)))
        (when-not (every? ids (:dependencies assumption))
          (reject! :missing-isolation-dependency))))
    required))

(defn declare!
  "Record explicit trust in the exact issued scope's open premises. This
   does not authenticate the declarant, prove privileges or exclude writers.
   No cross-generation transfer, provider contract, or admission authority."
  [node admission handle declaration]
  (try
    (boundaries!)
    (let [entry (when (instance? IsolationScope handle)
                  (locking scopes (.get scopes handle)))]
      (when-not (and entry (identical? node (:node entry))
                     (identical? admission (:admission entry)))
        (reject! :unissued-writer-isolation-scope))
      (let [{:keys [token root scope]} entry]
        (current! node admission token root)
        (let [required (select! scope declaration)]
          (boundaries!)
          (current! node admission token root)
          (merge base {:status :conditional-declared-writer-isolation
                       :scope scope :obligations required
                       :related-condition-ids related-conditions
                       :condition-link-semantics :related-not-entailing
                       :premise-acceptance :explicit-declaration-only
                       :deployment-authentication :not-established
                       :trusted-assumptions (set (map :premise-fingerprint required))}))))
    (catch clojure.lang.ExceptionInfo e
      (assoc base :status :not-established :reason (:reason (ex-data e))))))

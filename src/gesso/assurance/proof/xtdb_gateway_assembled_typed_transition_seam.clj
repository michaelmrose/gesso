(ns gesso.assurance.proof.xtdb-gateway-assembled-typed-transition-seam
  "v1758: A source-relative join from one current ApplicationAssembly, its
   issued certified handlers and one registered in-process Ring router to the
   v1757 declared-catalog-bound, one-shot guarded XTDB typed-transition seam.

   Check the EXACT closed operation set across three independently constructed
   views (original typed catalog, current assembled operations, certified-plan
   operations) and all currently declared certified route IDs. Bind one
   selected literal operation and its actual issued-handler/plan route into the
   physical preparer; recheck the live in-process router, contract and catalog
   at issuance, preparation AND the only submission attempt.

   A checked Ring router object does NOT establish that Biff/HTTP adopted it,
   that the supplied catalog is the application's authentic original policy,
   that all raw writers are registered, or that a formal theorem was proved.
   The existing XTDB gateway alone handles physical CAS. No release admission,
   principal authorization, or production certification is issued."
  (:require [gesso.assurance.proof.operation-set :as operation-set]
            [gesso.assurance.proof.installed-route-provenance :as routes]
            [gesso.assurance.proof.xtdb-gateway-typed-transition-seam :as typed]
            [gesso.live.application-preflight :as app]
            [gesso.live.application-preflight.certified-contract :as certified])
  (:import (java.util.concurrent.atomic AtomicBoolean)))

(def binding-type :gesso.assurance.proof.xtdb-gateway-assembled-typed-transition-seam/assembled-binding)
(def binding-version 1)

(def ^:private original-check certified/check-certified-contracts!)
(def ^:private original-summary app/operation-summary)
(def ^:private original-catalog-check operation-set/check-operation-set!)
(def ^:private original-router-description routes/describe-checked-ring-router)
(def ^:private original-typed-issuer typed/issue-source-bound-typed-transition-gateway!)

(defn- reject! [reason]
  (throw (ex-info "Unsupported application-bound typed XTDB transition."
                  {:xtdb-gateway-assembled-typed-transition-seam/rejection true
                   :reason reason})))

(defn- plain-map? [m]
  (and (or (instance? clojure.lang.PersistentArrayMap m)
           (instance? clojure.lang.PersistentHashMap m))
       (nil? (meta m))))

(defn- pinned! []
  (when-not (and (identical? original-check
                             (var-get #'certified/check-certified-contracts!))
                 (identical? original-summary (var-get #'app/operation-summary))
                 (identical? original-catalog-check
                             (var-get #'operation-set/check-operation-set!))
                 (identical? original-router-description
                             (var-get #'routes/describe-checked-ring-router))
                 (identical? original-typed-issuer
                             (var-get #'typed/issue-source-bound-typed-transition-gateway!)))
    (reject! :substituted-application-or-source-checker)))

(defn- current-source!
  [catalog contract-source router selected-operation]
  (pinned!)
  (when-not (and (plain-map? catalog)
                 (plain-map? contract-source)
                 (= #{:application-assembly :semantic-route-handlers
                      :certified-operations} (set (keys contract-source)))
                 (keyword? selected-operation) (namespace selected-operation))
    (reject! :invalid-application-bound-source))
  ;; Both checks independently re-evaluate the current source and executable
  ;; identities; neither a copied report nor metadata is an admission token.
  (let [original (try (original-catalog-check catalog)
                      (catch Exception _ (reject! :invalid-original-operation-catalog)))
        contract (try (original-check contract-source)
                      (catch Exception _ (reject! :uncertified-or-stale-application-source)))
        ring (try (original-router-description router contract-source)
                  (catch Exception _ (reject! :unregistered-or-stale-ring-router)))
        assembly (:application-assembly contract-source)
        summary (try (original-summary (:operation-acquisition-assembly assembly))
                     (catch Exception _ (reject! :invalid-assembled-operation-summary)))
        ids (:declared-operation-ids original)
        assembled (set (keys summary))
        certified-ids (set (keys (:certified-operations contract)))
        rows (vec (for [[operation info] summary
                        route (:routes info)]
                    (assoc route :operation operation)))
        route-ids (set (map :route-id rows))
        selected-routes (filterv #(= selected-operation (:operation %)) rows)]
    (when-not (and (= operation-set/report-type (:type original))
                   (= operation-set/operation-set-version (:version original))
                   (= :closed-declared-abstract-operation-set-only (:scope original))
                   (= :checked (:exact-declared-operation-coverage original))
                   (= :none (:authority original))
                   (= certified/report-type (:type contract))
                   (= certified/adapter-version (:version contract))
                   (= :none (:transaction-authority contract))
                   (= routes/report-type (:type ring))
                   (= routes/report-version (:version ring))
                   (= :none (:authority ring))
                   (= :not-established (:host-adoption ring))
                   (seq ids) (= ids assembled certified-ids)
                   (contains? ids selected-operation)
                   (empty? (:uncertified-operations contract))
                   (seq rows) (= (count rows) (count route-ids))
                   (= route-ids (:route-ids contract)
                      (:matched-certified-route-ids contract)
                      (:route-ids ring) (:certified-route-ids ring))
                   (seq selected-routes)
                   (every? (fn [{:keys [route-id method path]}]
                             (and (keyword? route-id) (namespace route-id)
                                  (keyword? method)
                                  (string? path) (.startsWith ^String path "/")
                                  (not (re-find #"[{}:*]" path))))
                           rows))
      (reject! :declared-catalog-assembly-or-router-closure-not-established))
    (pinned!)
    {:catalog original :contract contract :router ring
     :selected-routes (set (map (juxt :method :path) selected-routes))}))

(defn issue-assembled-source-bound-typed-transition-gateway!
  "Issue a preparer that binds ONE selected original operation to the entire
   currently checked in-process ApplicationAssembly, issued certified-plan
   handler table and registered Ring router. A request must select an EXACT
   literal route of this operation; no route/operation may be substituted on
   a later prepare or submit. The source catalog is captured at issuance.

   Public :prepare! takes [node resolver-issuance request candidate inputs
   field-map], never a replacement catalog, certified route or handler table.
   This is a bounded, rechecked APPLICATION-SNAPSHOT join, not proof of the
   deployed HTTP handler or authentic model rules."
  [source-catalog contract-source checked-router selected-operation]
  (let [initial (current-source! source-catalog contract-source checked-router
                                 selected-operation)
        source-bound (original-typed-issuer source-catalog selected-operation)
        source-bound-summary ((:summary! source-bound))
        _ (when-not (and (= typed/binding-type (:type source-bound))
                         (= typed/binding-version (:version source-bound))
                         (= :not-established (:installed-source-provenance
                                              source-bound-summary)))
            (reject! :invalid-source-bound-typed-gateway))
        recheck! (fn []
                   (when-not (= initial (current-source! source-catalog
                                                        contract-source checked-router
                                                        selected-operation))
                     (reject! :application-or-source-binding-changed))
                   true)
        prepare! (fn [node resolver-issuance request candidate inputs field-map]
                   (recheck!)
                   (when-not (and (plain-map? request)
                                  (contains? (:selected-routes initial)
                                             [(:request-method request)
                                              (:uri request)]))
                     (reject! :request-is-not-a-bound-operation-route))
                   (let [g ((:prepare! source-bound) node contract-source
                            resolver-issuance request candidate inputs field-map)
                         permit (AtomicBoolean. false)
                         submit! (fn []
                                   (when-not (.compareAndSet permit false true)
                                     (reject! :assembled-one-shot-already-attempted))
                                   (recheck!)
                                   ((:submit! g)))
                         summary! (fn []
                                    (assoc ((:summary! g))
                                           :assembled-source-binding
                                           :exact-current-assembly-certified-handlers-and-local-router
                                           :installed-router-adoption :not-established
                                           :original-business-policy-authenticity :not-established
                                           :complete-deployed-writer-coverage :not-established
                                           :production-certification :not-issued))]
                     (recheck!)
                     {:type typed/report-type :version typed/report-version
                      :submit! submit! :summary! summary!}))
        summary! (fn []
                   (recheck!)
                   {:type binding-type :version binding-version
                    :operation-id selected-operation
                    :assembled-source-binding
                    :exact-current-assembly-certified-handlers-and-local-router
                    :route-coverage :all-declared-literal-routes-rechecked
                    :source-operation-coverage :closed-declared-set-equals-assembly
                    :original-business-policy-authenticity :not-established
                    :installed-router-adoption :not-established
                    :complete-deployed-writer-coverage :not-established
                    :kernel-proof :not-established
                    :certificate :not-issued :admission :not-admitted
                    :authority :none})]
    (recheck!)
    {:type binding-type :version binding-version
     :prepare! prepare! :summary! summary!}))

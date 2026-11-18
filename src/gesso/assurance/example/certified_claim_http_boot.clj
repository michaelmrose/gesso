(ns gesso.assurance.example.certified-claim-http-boot
  "v1785: A concrete, opt-in BOOT ENTRY for the checked claim example.

   This application entry owns its operation ID, literal POST route, one
   checked contract, one selected HTTP adapter, and one catalog-derived
   typed/guarded physical trial. It invokes Gesso's v1784 configured closed
   startup as its ONLY HTTP startup path, instead of requiring the host to
   manually assemble four Biff modules or call unrestricted biff/start.

   The app fixture supplying the contract, inputs, raw adapter and XTDB node
   remains trusted application code. This is NOT an independently deployed
   service, authenticated business policy, completed proof certificate, or
   guarantee that other processes do not start unguarded listeners. The
   pre-existing v1495 physical reservation example is NOT compatible with
   certified-plan admission and is deliberately not relabeled as certified."
  (:require [gesso.assurance.proof.checked-biff-http-application-configuration
             :as configured]
            [gesso.assurance.proof.operation-set :as catalog]
            [gesso.live.application-preflight :as application]
            [gesso.live.application-preflight.certified-contract :as certified]))

(def operation-id :request/claim)
(def operation-route {:method :post :path "/operations/request/claim"
                      :route-id :request/claim})
(def report-type
  :gesso.assurance.example.certified-claim-http-boot/selected-application-boot)
(def report-version 1)

(def ^:private original-configured-issuer
  configured/issue-configured-checked-http-application!)
(def ^:private original-catalog-check catalog/check-operation-set!)
(def ^:private original-contract-check certified/check-certified-contracts!)
(def ^:private original-operation-summary application/operation-summary)

(defn- reject! [reason]
  (throw (ex-info "Checked claim example cannot issue its selected HTTP boot."
                  {:certified-claim-http-boot/rejection true :reason reason})))

(defn- plain-map? [m]
  (and (or (instance? clojure.lang.PersistentArrayMap m)
           (instance? clojure.lang.PersistentHashMap m))
       (nil? (meta m))))

(defn- pinned! []
  (when-not (and (identical? original-configured-issuer
                             (var-get #'configured/issue-configured-checked-http-application!))
                 (identical? original-catalog-check
                             (var-get #'catalog/check-operation-set!))
                 (identical? original-contract-check
                             (var-get #'certified/check-certified-contracts!))
                 (identical? original-operation-summary
                             (var-get #'application/operation-summary)))
    (reject! :substituted-original-claim-boot-boundary)))

(defn- check-source! [source]
  (pinned!)
  (when-not (and (plain-map? source)
                 (= #{:contract-source :catalog-source :inputs
                      :adapter-module :base-url!}
                    (set (keys source)))
                 (plain-map? (:contract-source source))
                 (plain-map? (:catalog-source source))
                 (= #{:catalog :physical-field-map}
                    (set (keys (:catalog-source source))))
                 (plain-map? (get-in source [:catalog-source :catalog]))
                 (plain-map? (get-in source [:catalog-source :physical-field-map]))
                 (plain-map? (:inputs source))
                 (fn? (:base-url! source)))
    (reject! :invalid-claim-example-boot-source))
  (let [contract (:contract-source source)
        catalog-source (get-in source [:catalog-source :catalog])
        checked (try (original-contract-check contract)
                     (catch Exception _ (reject! :claim-certified-contract-not-checked)))
        checked-catalog (try (original-catalog-check catalog-source)
                             (catch Exception _ (reject! :claim-original-catalog-not-checked)))
        route-map (try (original-operation-summary
                        (get-in contract [:application-assembly
                                          :operation-acquisition-assembly]))
                       (catch Exception _ (reject! :claim-assembly-not-checked)))
        routes (get-in route-map [operation-id :routes])
        fields (mapv #(select-keys % [:method :path :route-id]) routes)]
    (when-not (and (= certified/report-type (:type checked))
                   (= catalog/report-type (:type checked-catalog))
                   (= #{operation-id} (set (keys route-map))
                      (set (keys (:certified-operations contract)))
                      (set (keys (:certified-operations checked)))
                      (:declared-operation-ids checked-catalog))
                   (empty? (:uncertified-operations checked))
                   (= [operation-route] fields)
                   (= #{operation-id}
                      (set (keys (:semantic-route-handlers contract))))
                   (= :none (:transaction-authority checked)
                      (:authority checked-catalog))
                   (= :none (:certificate checked))
                   (= :not-issued (:certificate checked-catalog)))
      (reject! :claim-route-contract-or-catalog-diverged))
    (pinned!)
    true))

(defn- report [status]
  {:type report-type :version report-version
   :operation-id operation-id
   :selected-http-route :post-request-claim
   :status status
   :boot-entry :concrete-claim-app-configured-closed-start
   :module-inventory :derived-only-by-v1784
   :typed-atomic-trial :catalog-bound-selected-operation
   :local-http-adapter-consumption
   (if (= :verified-configured-start status)
     :verified-during-actual-application-boot
     :not-established)
   :authentic-business-policy :not-established
   :deployed-server-identity :not-established
   :independent-startup-coverage :not-established
   :principal-authentication :not-established
   :authenticated-xtdb-receipt :not-established
   :actual-live-delivery :not-established
   :complete-writer-coverage :not-established
   :certificate :not-issued :admission :not-admitted
   :retry-authority :none :transaction-authority :none :authority :none})

(defn issue-claim-http-boot!
  "Concrete claim-example boot. Applications supply the checked claim contract,
   exact catalog+field projection, invocation inputs, selected adapter and
   loopback locator. Callers do NOT supply module order, Biff system/handler,
   a generic execution thunk, or a transaction. The example entry always
   enables the catalog-derived typed/guarded trial on its selected POST.

   The returned :boot! takes NO arguments. It calls the original v1784
   configured one-shot startup with {} as the initial system; :stop! delegates
   to its original managed cleanup and :summary! is redacted. Any failure
   consumes the issued boot; the selected adapter's physical state after an
   uncertain failure remains an explicitly open question."
  [source]
  (check-source! source)
  (let [issued (original-configured-issuer
                {:contract-source (:contract-source source)
                 :adapter-module (:adapter-module source)
                 :base-url! (:base-url! source)
                 :typed-trial {:catalog-source (:catalog-source source)
                               :operation operation-id
                               :inputs (:inputs source)}})
        summary! (fn []
                   (pinned!)
                   (report (:status ((:summary! issued)))))
        boot! (fn []
                (check-source! source)
                (let [started ((:start-once! issued) {})]
                  (when-not (and (= configured/report-type
                                    (get-in started [:report :type]))
                                 (= :verified-configured-start
                                    (get-in started [:report :status]))
                                 (= :none (get-in started [:report :authority])))
                    (reject! :unqualified-claim-app-startup))
                  (report (:status (:report started)))))
        stop! (fn []
                (pinned!)
                (report (:status ((:stop! issued)))))]
    {:boot! boot! :stop! stop! :summary! summary!}))

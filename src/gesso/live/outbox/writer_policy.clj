(ns gesso.live.outbox.writer-policy
  "Restricted synchronous adapter policy for a selected mapped executor.
   Domain submissions require a genuine framed handoff; initialization is
   exact and one-use. Maintenance is confined to a single infrastructure
   put with assertion guards and a trusted exact-footprint classifier.
   This is local adapter mediation, not closure over raw XTDB writers."
  (:require [gesso.live.outbox.model-publication :as publication]
            [gesso.model.inductive-plan :as inductive]))

(def ^:private maintenance-kinds
  {:gesso_publication_manifests :manifest
   :gesso_live_outbox :worker})

(defn- reject! [reason]
  (throw (ex-info "Invalid mapped writer policy." {:reason reason})))

(defn- put-op? [op]
  (and (vector? op) (<= 3 (count op)) (= :put-docs (first op))
       (simple-keyword? (second op))
       (re-matches #"[a-z][a-z0-9_]*" (name (second op)))
       (every? map? (subvec op 2))))

(defn- initializer? [ops]
  (and (vector? ops) (seq ops)
       (every? #(and (put-op? %)
                     (not= :gesso_commands (second %))
                     (not (contains? maintenance-kinds (second %)))) ops)))

(defn validate-config!
  "Validate the exact selected policy inputs before acquiring a node.
   The optional initializer is an exact trusted vector, not a general
   writer grant. The maintenance classifier and observer are trusted."
  [{:keys [admission initializer maintenance! observe!] :as config}]
  (when-not (and (map? config)
                 (= #{:admission :initializer :maintenance! :observe!} (set (keys config)))
                 (or (nil? initializer) (initializer? initializer))
                 (fn? maintenance!) (fn? observe!))
    (reject! :invalid-mapped-writer-policy))
  (inductive/describe-mapped-admission admission)
  config)

(defn- maintenance-kind [ops]
  (let [op (peek ops)]
    (when (and (put-op? op) (= 3 (count op)) (seq (pop ops))
               (every? #(and (vector? %) (string? (first %))
                             (re-matches #"ASSERT [^;\r\n]+" (first %))) (pop ops)))
      (get maintenance-kinds (second op)))))

(defn build-policy!
  "Bind admission to the owned connectable. Return the literal-boolean
   predicate expected by the XTDB adapter. The maintenance callback must
   check the precise infrastructure transition. Observer failure refuses
   submission. Initialization is consumed on admission, not on commit."
  [connectable config]
  (let [{:keys [admission initializer maintenance! observe!]} (validate-config! config)
        initialized? (atom false)]
    (when-not connectable (reject! :missing-mapped-writer-connectable))
    (fn [{:keys [api tx-ops tx-options] :as envelope}]
      (boolean
       (when (and (= :execute-tx api) (empty? tx-options)
                  (vector? tx-ops) (seq tx-ops))
         (let [kind (or (when-let [{:keys [identity candidate]}
                                  (publication/claim-framed-submission! admission connectable envelope)]
                          (assoc identity :kind :mapped-operation :candidate candidate))
                        (when (and initializer (= initializer tx-ops)
                                   (compare-and-set! initialized? false true))
                          {:kind :initializer})
                        (when-let [expected (maintenance-kind tx-ops)]
                          (let [checked (try (maintenance! tx-ops)
                                             (catch clojure.lang.ExceptionInfo _ nil))]
                            (when (= expected (:kind checked)) checked))))]
           (when kind
             (observe! kind)
             true)))))))

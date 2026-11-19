(ns gesso.assurance.proof.xtdb-gateway-issued-operator-witness-binding
  "v1879: Derive a private source-relative expected Ed25519 statement binding
   from ONE original checked issued-executor prestate witness, its original
   certified proof origin, and a SEPARATELY completed pinned XTDB read.

   This pure checker is not a witness authenticator. ONLY the original issued
   collector may treat its own private captured values as checked; passing
   copied maps to this function creates no independent physical evidence.
   Neither reported database identity nor any signature proves deployment,
   history retention, erasure exclusion, ABA safety, or transaction authority."
  (:require [gesso.assurance.proof.xtdb-gateway-issued-snapshot-identity
             :as snapshot]
            [gesso.assurance.proof.xtdb-gateway-issued-operator-claim-intake
             :as intake]))

(defn- reject! [reason]
  (throw (ex-info "Private operator claim witness binding not established."
                  {:reason reason})))

(defn- plain? [x]
  (and (map? x) (nil? (meta x))))

(def ^:private origin-fields
  [:proof-fingerprint :proof-source-fingerprint :proof-subject-fingerprint
   :dependency-graph-fingerprint :transition-fingerprint])

(defn expected-private-operator-binding!
  "Produces ONLY an expected binding, never an admitted operator claim.
   Enforces the exact original target and strict same-database ordered bases.
   No external caller may substitute these values at the collector boundary."
  [operation origin original-witness later-witness]
  (let [target (:target original-witness)
        original-point (:original-snapshot-point original-witness)
        later-point (:point later-witness)
        table (first target)
        row-id (second target)
        selected-source (:source-identity original-witness)
        required-origin (set (conj origin-fields :route-ids))]
    (when-not (and (qualified-keyword? operation)
                   (plain? origin)
                   (= required-origin (set (keys origin)))
                   (every? #(and (string? (get origin %))
                                 (<= 1 (count (get origin %)) 1024)) origin-fields)
                   (set? (:route-ids origin)) (seq (:route-ids origin))
                   (plain? original-witness)
                   (= #{:operation-id :target :before :original-snapshot-point
                        :source-identity} (set (keys original-witness)))
                   (= operation (:operation-id original-witness))
                   (= (:proof-fingerprint origin) (:proof-fingerprint selected-source))
                   (= (:transition-fingerprint origin)
                      (:transition-fingerprint selected-source))
                   (plain? selected-source)
                   (vector? target) (= 2 (count target))
                   (keyword? table) (nil? (namespace table))
                   (re-matches #"[a-z][a-z0-9_]*" (name table))
                   (string? row-id) (<= 1 (count row-id) 1024)
                   (plain? (:before original-witness))
                   (= row-id (get-in original-witness [:before :xt/id]))
                   (snapshot/valid-private-point? original-point)
                   (plain? later-witness)
                   (= #{:target :point} (set (keys later-witness)))
                   (= target (:target later-witness))
                   (snapshot/valid-private-point? later-point))
      (reject! :incomplete-private-original-and-later-pinned-witness))
    (when-not (= :later-reported-database-basis
                 (snapshot/reported-basis-relation original-point later-point))
      (reject! :operator-claim-requires-advanced-same-database-pinned-interval))
    (let [binding (merge {:operation-id operation
                          :database-instance (:database original-point)
                          :physical-table table :physical-row-id row-id
                          :system-from (:system-time original-point)
                          :system-to (:system-time later-point)}
                         (select-keys origin origin-fields))]
      (when-not (= (set intake/binding-fields) (set (keys binding)))
        (reject! :invalid-private-operator-history-claim-binding))
      binding)))

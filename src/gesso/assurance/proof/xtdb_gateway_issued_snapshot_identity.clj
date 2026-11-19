(ns gesso.assurance.proof.xtdb-gateway-issued-snapshot-identity
  "v1872: Comparable database/time points extracted from ORIGINAL decoded XTDB
   snapshot tokens. These are observations about reported snapshot bases, NOT
   proofs of row history, transaction-time atomicity, or no ABA."
  (:import (java.time Instant)))

(defn comparable-point
  "Reduce an already decoded XTDB token to one database and one Instant,
   or nil when its basis cannot be placed in a single comparable timeline.
   Never retain or publish the original token."
  [decoded]
  (when (and (map? decoded) (= 1 (count decoded)))
    (let [[database times] (first decoded)]
      (when (and (string? database) (seq database)
                 (sequential? times) (= 1 (count times))
                 (instance? Instant (first times)))
        {:database database :system-time (first times)}))))

(defn valid-private-point?
  [point]
  (and (map? point) (nil? (meta point))
       (= #{:database :system-time} (set (keys point)))
       (string? (:database point)) (seq (:database point))
       (instance? Instant (:system-time point))))

(defn reported-basis-relation
  "Classify two already pinned reads. Even a later reported database basis
   does NOT imply that the selected row changed, and an equal basis does NOT
   independently attest a continuously held row-version invariant."
  [historical later]
  (cond
    (not (and (valid-private-point? historical)
              (valid-private-point? later))) :not-comparable
    (not= (:database historical) (:database later)) :different-reported-databases
    :else (let [comparison (compare (:system-time historical)
                                    (:system-time later))]
            (cond (neg? comparison) :later-reported-database-basis
                  (pos? comparison) :earlier-reported-database-basis
                  :else :same-reported-database-basis))))

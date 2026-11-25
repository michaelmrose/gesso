(ns gesso.assurance.reference-equal-time-merge-test
  "Characterize the pinned physical merge primitives, not a replay model."
  (:require [clojure.test :refer [deftest is]])
  (:import (java.util Comparator PriorityQueue)
           (org.apache.arrow.memory RootAllocator)
           (org.apache.arrow.vector.types.pojo ArrowType$Int)
           (xtdb.arrow DenseUnionVector FixedSizeBinaryVector LongVector Relation)
           (xtdb.bitemporal Polygon PolygonCalculator)
           (xtdb.trie EventRowPointer)
           (xtdb.util TemporalBounds TemporalDimension)))

(defn- leaf [allocator rows]
  (let [iid (FixedSizeBinaryVector. allocator "_iid" false 16)
        system (LongVector. allocator "_system_from" false)
        from (LongVector. allocator "_valid_from" false)
        to (LongVector. allocator "_valid_to" false)
        op (DenseUnionVector. allocator "op")
        rel (Relation. allocator [iid system from to op] 0)]
    (try
      (doseq [[id s f t value] rows]
        (let [bytes (byte-array 16)]
          (aset-byte bytes 15 (byte id))
          (.writeObject iid bytes))
        (.writeLong system s)
        (.writeLong from f)
        (.writeLong to t)
        ;; A synthetic scalar put leg labels rows; the polygon calculator
        ;; only consults the leg name and temporal coordinates.
        (.writeLong (.vectorFor op "put" (ArrowType$Int. 64 true) false) value)
        (.endRow rel))
      rel
      (catch Throwable e (.close rel) (throw e)))))

(defn- pointer [rel] (EventRowPointer. rel (byte-array 0)))
(defn- comparator* [] (.comparator EventRowPointer/Companion))

(defn- scan-point [leaves]
  (let [bounds (TemporalBounds. (TemporalDimension/at 0) (TemporalDimension/at 10))
        calc (PolygonCalculator. bounds)
        queue (PriorityQueue. ^Comparator (comparator*))
        pairs (mapv (fn [rel] [(pointer rel) rel]) leaves)
        relations (into {} pairs)]
    (doseq [[p _] pairs] (.add queue p))
    (loop [order [] visible []]
      (if-let [^EventRowPointer p (.poll queue)]
        (let [^Relation rel (get relations p)
              value (.getLong (.get rel "op") (.getIndex p))
              ^Polygon polygon (.calculate calc p)
              system (.getSystemFrom p)
              visible? (and polygon
                            (some (fn [i]
                                    (let [f (.getValidFrom polygon i)
                                          t (.getValidTo polygon i)
                                          s-to (.getSystemTo polygon i)]
                                      (and (not= f t) (not= system s-to)
                                           (.intersects bounds f t system s-to))))
                                  (range (.getValidTimeRangeCount polygon))))]
          (.nextIndex p)
          (when (.isValid p) (.add queue p))
          (recur (conj order value) (cond-> visible visible? (conj value))))
        {:order order :visible visible}))))

(deftest equal-system-time-physical-merge-ties
  (with-open [allocator (RootAllocator.)
              prior (leaf allocator [[1 10 1 10 2] [1 10 -10 10 1]])
              first-write (leaf allocator [[1 10 -2 2 3]])
              both-writes (leaf allocator [[1 10 1 5 4] [1 10 -2 2 3]])
              later (leaf allocator [[1 11 -2 2 5]])
              other (leaf allocator [[2 10 -2 2 6]])]
    (let [c (comparator*) p (pointer prior) q (pointer first-write)]
      (is (zero? (.compare c p q)))
      (is (zero? (.compare c q p)))
      (is (neg? (.compare c (pointer later) p)))
      (is (neg? (.compare c p (pointer other))))
      (.nextIndex p)
      (is (zero? (.compare c p q))))
    (is (= {:order [2 3 1] :visible [3]}
           (scan-point [prior first-write])))
    (is (= {:order [2 4 1 3] :visible [1]}
           (scan-point [prior both-writes])))
    (is (= {:order [4 2 3 1] :visible [3]}
           (scan-point [both-writes prior])))))

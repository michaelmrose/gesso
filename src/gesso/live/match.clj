(ns gesso.live.match
  "Generic matcher contract and index helpers for gesso.live."
  (:require
   [clojure.set :as set]))

(defn- matcher-fn
  [matcher k]
  (let [f (get matcher k)]
    (when-not (fn? f)
      (throw (ex-info (str "Matcher missing required fn: " k)
                      {:matcher-key k})))
    f))

(defn index-entries
  "Return matcher-derived index entries for a subscription."
  [matcher subscription]
  (let [f (matcher-fn matcher :subscription->entries)
        entries (f subscription)]
    (vec (or entries []))))

(defn candidate-entries
  "Return matcher-derived candidate lookup entries for a changed value."
  [matcher ctx changed]
  (let [f (matcher-fn matcher :changed->entries)
        entries (f ctx changed)]
    (vec (or entries []))))

(defn confirm-match?
  "Return whether a subscription truly matches a changed value.

   If matcher has no :matches? fn, defaults to true."
  [matcher ctx changed subscription]
  (if-let [f (:matches? matcher)]
    (boolean (f ctx changed subscription))
    true))

(defn candidate-subscriber-ids
  "Return the union of subscriber ids found under the provided index entries."
  [indexes entries]
  (reduce
   (fn [acc [index-name index-key]]
     (let [ids (get-in indexes [index-name index-key] #{})]
       (set/union acc ids)))
   #{}
   entries))

(defn- candidate-subscribers
  [subscribers candidate-ids]
  (keep #(get subscribers %) candidate-ids))

(defn matching-subscriber-ids
  "Run the full matching pipeline.

   Expected input map keys:
   - :matcher
   - :indexes
   - :subscribers
   - :ctx
   - :changed"
  [{:keys [matcher indexes subscribers ctx changed]}]
  (let [entries        (candidate-entries matcher ctx changed)
        candidate-ids  (candidate-subscriber-ids indexes entries)
        candidates     (candidate-subscribers subscribers candidate-ids)]
    (reduce
     (fn [acc {:keys [subscriber/id subscription]}]
       (if (confirm-match? matcher ctx changed subscription)
         (conj acc id)
         acc))
     #{}
     candidates)))

(defn- add-entry
  [indexes subscriber-id [index-name index-key]]
  (update-in indexes [index-name index-key] (fnil conj #{}) subscriber-id))

(defn index-subscription
  "Index one subscriber under all matcher-derived index entries."
  [indexes matcher subscriber]
  (let [subscriber-id (:subscriber/id subscriber)
        subscription  (:subscription subscriber)
        entries       (index-entries matcher subscription)]
    (reduce
     (fn [acc entry]
       (add-entry acc subscriber-id entry))
     indexes
     entries)))

(defn- remove-entry
  [indexes subscriber-id [index-name index-key]]
  (let [bucket     (get-in indexes [index-name index-key] #{})
        bucket'    (disj bucket subscriber-id)
        by-name    (get indexes index-name {})]
    (cond
      (seq bucket')
      (assoc-in indexes [index-name index-key] bucket')

      (seq (dissoc by-name index-key))
      (update indexes index-name dissoc index-key)

      :else
      (dissoc indexes index-name))))

(defn unindex-subscription
  "Remove one subscriber from all matcher-derived index entries."
  [indexes matcher subscriber]
  (let [subscriber-id (:subscriber/id subscriber)
        subscription  (:subscription subscriber)
        entries       (index-entries matcher subscription)]
    (reduce
     (fn [acc entry]
       (remove-entry acc subscriber-id entry))
     indexes
     entries)))

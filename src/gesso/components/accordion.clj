(ns gesso.components.accordion
  (:require
   [clojure.string :as str]
   [gesso.util :refer :all]))

;; -----------------------------------------------------------------------------
;; Helpers
;; -----------------------------------------------------------------------------

(defn- normalize-component-args
  "Returns [opts children] for variadic component calls.
   If the first arg is a map, it is treated as opts; otherwise opts is {} and
   all args are treated as children."
  [args]
  (let [[opts & children] args
        opts (if (map? opts) opts {})]
    [opts (if (map? (first args)) children args)]))

(defn- hs-lines
  "Join non-nil lines into a multiline hyperscript string."
  [& lines]
  (->> lines
       flatten
       (remove nil?)
       (str/join "\n")))

(defn- accordion-chevron-script []
  "set chev to first <span[data-accordion-chevron]/> in me
  if chev
    if me.open put '▴' into chev else put '▾' into chev end
  end")

(defn- accordion-single-script [collapsible?]
  (if collapsible?
    "set root to closest <div[data-accordion-root]/>
    if me.open
        for d in <details/> in root
            if d != me set d.open to false end
        end
    end"
    "set root to closest <div[data-accordion-root]/>
    if me.open
        for d in <details/> in root
            if d != me set d.open to false end
        end
    else
        set anyOpen to false
        for d in <details/> in root
            if d.open set anyOpen to true end
        end
    if not anyOpen set me.open to true end
    end"))


(defn- accordion-script
  [{:keys [type collapsible?]}]
  (let [type (or type :multiple)
        collapsible? (if (nil? collapsible?) true collapsible?)]
    (case type
      :single
      (hs-lines
       "on toggle"
       (accordion-single-script collapsible?)
       (accordion-chevron-script))

      :multiple
      (hs-lines
       "on toggle"
       (accordion-chevron-script)))))

(defn- add-accordion-script [attrs script]
  (if script
    (merge-attrs attrs {:_ script})
    attrs))

(defn- add-script-to-details-child [child script]
  (if (and script (vector? child) (= :details (first child)))
    (let [[tag maybe-attrs & kids] child
          has-attrs? (map? maybe-attrs)
          attrs (if has-attrs? maybe-attrs {})
          kids  (if has-attrs? kids (cons maybe-attrs kids))]
      (into [tag (add-accordion-script attrs script)] kids))
    child))

(defn- add-script-to-details-children [children script]
  (map #(add-script-to-details-child % script) children))

(defn- prepare-accordion-items
  [{:keys [items item-fn type
           default-value default-values
           default-index default-indexes]}]
  (let [type (or type :multiple)
        items* (if (and item-fn items)
                 (map item-fn items)
                 items)
        items* (vec (or items* []))
        default-one (cond
                      (some? default-value)
                      (->value default-value default-value)

                      (some? default-index)
                      (when-let [item (nth items* default-index nil)]
                        (->value (:value item) (str "item-" (inc default-index))))

                      :else nil)
        default-many (cond
                       (seq default-values)
                       (->> default-values
                            (map #(->value % %))
                            set)

                       (seq default-indexes)
                       (->> default-indexes
                            (keep (fn [idx]
                                    (when-let [item (nth items* idx nil)]
                                      (->value (:value item) (str "item-" (inc idx))))))
                            set)

                       :else #{})]
    (map-indexed
     (fn [i item]
       (let [value (->value (:value item) (str "item-" (inc i)))
             item  (assoc item :value value)]
         (cond
           (contains? item :open?) item
           (= type :single)   (assoc item :open? (= value default-one))
           (= type :multiple) (assoc item :open? (contains? default-many value))
           :else item)))
     items*)))

;; -----------------------------------------------------------------------------
;; Components
;; -----------------------------------------------------------------------------

(defn accordion-content [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))]
      (el :section
          {:class (class-names "p-4 pt-0 text-gray-600" class)}
          attrs
          (nodes (:children props))))
    (let [[opts children] (normalize-component-args args)
          {:keys [class attrs]} (split-opts opts)]
      (el :section
          {:class (class-names "p-4 pt-0 text-gray-600" class)}
          attrs
          children))))


(defn accordion-trigger
  "Accordion trigger: emits <summary>.

  Adds:
  - decent default spacing/hover
  - a chevron that flips ▾ / ▴ on toggle (via Hyperscript)"
  [& args]
  (let [base-class "cursor-pointer w-full font-medium p-4 hover:bg-gray-50 list-none flex justify-between items-center gap-3"]
    (if (only-map-arg? args)
      (let [{:keys [props class attrs]} (split-opts (first args))
            text (:text props)
            chevron? (not= false (:chevron? props))]
        (el :summary
            {:class (class-names base-class class)}
            attrs
            (if chevron?
              [[:span {:class "flex-1 text-left w-full"} text]
               [:span {:data-accordion-chevron true
                       :aria-hidden "true"
                       :class "text-xl leading-none"}
                "▾"]]
              (nodes text))))
      (let [[opts children] (normalize-component-args args)
            {:keys [props class attrs]} (split-opts opts)
            chevron? (not= false (:chevron? props))]
        (el :summary
            {:class (class-names base-class class)}
            attrs
            (if chevron?
              [(into [:span {:class "flex-1 text-left min-w-0"}]
                     (normalize-children children))
               [:span {:data-accordion-chevron true
                       :aria-hidden "true"
                       :class "text-xl leading-none"}
                "▾"]]
              children))))))

(defn accordion-item
  "Accordion item: emits <details>.

  Map-only form:
    (accordion-item {:value \"item-1\"
                     :title \"...\"
                     :content [...]
                     :open? true|false
                     :disabled? true|false
                     :trigger-opts {...}
                     :content-opts {...}
                     :attrs {...}
                     :class \"...\"})

  Long form:
    (accordion-item {:value ... :open? ... :disabled? ... :class ... :attrs ...}
      (accordion-trigger ...)
      (accordion-content ...))"
  [& args]
  (let [base-class "border-b last:border-b-0 group"]
    (if (only-map-arg? args)
      (let [{:keys [props class attrs]} (split-opts (first args))
            {:keys [value title content open? disabled? trigger-opts content-opts]} props
            v (->value value "item")
            details-attrs (merge-attrs
                           {:class (class-names
                                    base-class
                                    (when disabled? "opacity-60 pointer-events-none")
                                    class)
                            :open (when open? true)
                            :data-accordion-value v}
                           attrs)]
        (el :details
            {}
            details-attrs
            [(accordion-trigger (merge {:chevron? true
                                        :text title}
                                       trigger-opts))
             (apply accordion-content content-opts (nodes content))]))
      (let [[opts children] (normalize-component-args args)
            {:keys [props class attrs]} (split-opts opts)
            {:keys [value open? disabled?]} props
            v (->value value "item")
            details-attrs (merge-attrs
                           {:class (class-names
                                    base-class
                                    (when disabled? "opacity-60 pointer-events-none")
                                    class)
                            :open (when open? true)
                            :data-accordion-value v}
                           attrs)]
        (el :details {} details-attrs children)))))

(defn accordion
  "Accordion root.

  Supported call styles:

  1) Map-only:
     (accordion {:items [...]})

  2) Map-only with item-fn:
     (accordion {:items coll :item-fn (fn [x] {:value ... :title ... :content ...})})

  3) Function + items:
     (accordion (fn [x] {:value ... :title ... :content ...}) coll)

  4) Options + function + items:
     (accordion {:type :single :default-value \"item-2\"} (fn [x] ...) coll)

  5) Long form children:
     (accordion {:type :multiple} (accordion-item ...) ...)

  Shadcn-ish options:
    :type             :single | :multiple (default :multiple)
    :default-value    for :single
    :default-values   for :multiple
    :default-index    for :single, zero-based
    :default-indexes  for :multiple, zero-based
    :collapsible?     for :single (default true)

  Notes:
  - We implement :single + :collapsible? using Hyperscript on toggle.
  - We also flip the chevron ▾ / ▴ on toggle.
  - Value-based defaults take precedence over index-based defaults."
  [& args]
  (let [root-class "border rounded-lg bg-white overflow-hidden shadow-sm"]
    (cond
      ;; (accordion {:items ...})
      (only-map-arg? args)
      (let [{:keys [props class attrs]} (split-opts (first args))
            {:keys [items item-fn type
                    default-value default-values
                    default-index default-indexes
                    collapsible?]} props
            script (accordion-script {:type type :collapsible? collapsible?})
            items* (prepare-accordion-items
                    {:items items
                     :item-fn item-fn
                     :type type
                     :default-value default-value
                     :default-values default-values
                     :default-index default-index
                     :default-indexes default-indexes})]
        (el :div
            {:class (class-names root-class class)
             :data-accordion-root true}
            attrs
            (for [item items*]
              (let [item-attrs (get-in item [:attrs])
                    item (assoc item :attrs (add-accordion-script item-attrs script))]
                (accordion-item item)))))

      ;; (accordion item-fn items)
      (and (= 2 (count args))
           (fn? (first args))
           (sequential? (second args)))
      (accordion {:item-fn (first args) :items (second args)})

      ;; (accordion opts item-fn items)
      (and (= 3 (count args))
           (map? (first args))
           (fn? (second args))
           (sequential? (nth args 2)))
      (accordion (assoc (first args) :item-fn (second args) :items (nth args 2)))

      ;; Long form: (accordion opts children...)
      :else
      (let [[opts children] (normalize-component-args args)
            {:keys [props class attrs]} (split-opts opts)
            {:keys [type collapsible?]} props
            script (accordion-script {:type type :collapsible? collapsible?})
            children (add-script-to-details-children children script)]
        (el :div
            {:class (class-names root-class class)
             :data-accordion-root true}
            attrs
            children)))))

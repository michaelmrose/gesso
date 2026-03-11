(ns gesso.components.card
  (:require
   [clojure.string :as str]
   [gesso.util :refer :all]))

(defn card-title [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))]
      (el :h2 {:class class} attrs (nodes (:text props))))
    (let [[opts & children] args
          opts (if (map? opts) opts {})
          {:keys [class attrs]} (split-opts opts)
          children (if (map? (first args)) children args)]
      (el :h2 {:class class} attrs children))))

(defn card-description [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))]
      (el :p {:class class} attrs (nodes (:text props))))
    (let [[opts & children] args
          opts (if (map? opts) opts {})
          {:keys [class attrs]} (split-opts opts)
          children (if (map? (first args)) children args)]
      (el :p {:class class} attrs children))))

(defn card-header [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))
          {:keys [title description children]} props]
      (el :header {:class class} attrs
          [(when title (card-title {} title))
           (when description (card-description {} description))
           (nodes children)]))
    (let [[opts & children] args
          opts (if (map? opts) opts {})
          {:keys [class attrs]} (split-opts opts)
          children (if (map? (first args)) children args)]
      (el :header {:class class} attrs children))))

(defn card-content [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))]
      (el :section {:class class} attrs (nodes (:children props))))
    (let [[opts & children] args
          opts (if (map? opts) opts {})
          {:keys [class attrs]} (split-opts opts)
          children (if (map? (first args)) children args)]
      (el :section {:class class} attrs children))))

(defn card-footer [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))]
      (el :footer {:class class} attrs (nodes (:children props))))
    (let [[opts & children] args
          opts (if (map? opts) opts {})
          {:keys [class attrs]} (split-opts opts)
          children (if (map? (first args)) children args)]
      (el :footer {:class class} attrs children))))


(defn card
  "Long form:
    (card {:class ... :attrs ...} children...)

  Short form (map-only):
    (card {:class ... :attrs ... :header <node> :title <node> :description <node> :content <node|seq> :footer <node|seq>})"
  [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))
          {:keys [header title description content footer]} props
          header-node (or header
                          (when (or title description)
                            (card-header {}
                              (when title (card-title {} title))
                              (when description (card-description {} description)))))
          content-node (when (some? content)
                         (apply card-content {} (nodes content)))
          footer-node (when (some? footer)
                        (apply card-footer {} (nodes footer)))]
      (el :div {:class (class-names "card" class)} attrs
          [header-node content-node footer-node]))
    (let [[opts & children] args
          opts (if (map? opts) opts {})
          {:keys [class attrs]} (split-opts opts)
          children (if (map? (first args)) children args)]
      (el :div {:class (class-names "card" class)} attrs children))))

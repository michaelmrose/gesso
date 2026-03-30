(ns gesso.components.text
  (:require [gesso.util :refer :all]))

(def ^:private text-variant-classes
  {:body "font-body text-base-theme leading-body"
   :muted "font-body text-base-theme leading-body text-muted-foreground"
   :small "font-body text-sm-theme leading-body"
   :caption "font-body text-xs-theme leading-body text-muted-foreground"
   :label "font-body text-sm-theme leading-tight-theme font-medium"
   :lead "font-body text-lg-theme leading-loose-theme"})

(def ^:private heading-level-classes
  {1 "font-heading text-3xl-theme leading-tight-theme tracking-tight-theme font-bold"
   2 "font-heading text-2xl-theme leading-tight-theme tracking-tight-theme font-semibold"
   3 "font-heading text-xl-theme leading-tight-theme tracking-tight-theme font-semibold"
   4 "font-heading text-lg-theme leading-tight-theme tracking-tight-theme font-semibold"
   5 "font-heading text-md-theme leading-tight-theme tracking-tight-theme font-medium"
   6 "font-heading text-base-theme leading-tight-theme tracking-tight-theme font-medium"})

(def ^:private heading-size-classes
  {:xs "text-xs-theme"
   :sm "text-sm-theme"
   :base "text-base-theme"
   :md "text-md-theme"
   :lg "text-lg-theme"
   :xl "text-xl-theme"
   :2xl "text-2xl-theme"
   :3xl "text-3xl-theme"})

(defn- merge-wrapper-opts
  "Merge wrapper defaults into the first arg when it is an opts map.
   Otherwise prepend the wrapper defaults as the opts map."
  [args wrapper-opts]
  (if (and (seq args) (map? (first args)))
    (cons (merge wrapper-opts (first args)) (rest args))
    (cons wrapper-opts args)))

(defn text
  "Semantic text wrapper.

  Short form:
    (text {:variant :muted
           :as :p
           :text \"Saved successfully.\"})

  Long form:
    (text {:variant :lead}
      \"Longer introductory copy\")

  Supported variants:
    :body :muted :small :caption :label :lead"
  [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))
          {:keys [variant as text]} props
          variant-class (get text-variant-classes (or variant :body))
          tag (or as :p)]
      (el tag
          {:class (class-names variant-class class)}
          attrs
          (nodes text)))
    (let [[opts children] (normalize-component-args args)
          {:keys [props class attrs]} (split-opts opts)
          {:keys [variant as]} props
          variant-class (get text-variant-classes (or variant :body))
          tag (or as :p)]
      (el tag
          {:class (class-names variant-class class)}
          attrs
          children))))

(defn heading
  "Semantic heading wrapper.

  Short form:
    (heading {:level 2
              :text \"Employee Dashboard\"})

    (heading {:level 3
              :size :xl
              :as :h2
              :text \"Visually larger heading\"})

  Long form:
    (heading {:level 2} \"Section title\")

  Defaults:
    :level => 2
    :as    => matching hN tag
    :size  => nil (use level-based size)"
  [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))
          {:keys [level size as text]} props
          level (or level 2)
          base-class (get heading-level-classes level (heading-level-classes 2))
          size-class (get heading-size-classes size)
          tag (or as (keyword (str "h" level)))]
      (el tag
          {:class (class-names base-class size-class class)}
          attrs
          (nodes text)))
    (let [[opts children] (normalize-component-args args)
          {:keys [props class attrs]} (split-opts opts)
          {:keys [level size as]} props
          level (or level 2)
          base-class (get heading-level-classes level (heading-level-classes 2))
          size-class (get heading-size-classes size)
          tag (or as (keyword (str "h" level)))]
      (el tag
          {:class (class-names base-class size-class class)}
          attrs
          children))))

(defn page-title
  [& args]
  (apply heading (merge-wrapper-opts args {:level 1})))

(defn section-title
  [& args]
  (apply heading (merge-wrapper-opts args {:level 2})))

(defn muted-text
  [& args]
  (apply text (merge-wrapper-opts args {:variant :muted})))

(defn label-text
  [& args]
  (apply text (merge-wrapper-opts args {:variant :label
                                        :as :span})))

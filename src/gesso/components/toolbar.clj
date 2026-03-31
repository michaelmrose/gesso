(ns gesso.components.toolbar
  (:require [gesso.util :refer :all]))

(defn toolbar-start
  "Leading toolbar region.

  Short form:
    (toolbar-start {:children [...]})

  Long form:
    (toolbar-start
      ...)"
  [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))]
      (el :div
          {:class (class-names "flex flex-wrap items-center gap-toolbar" class)}
          (merge-attrs attrs {:data-toolbar-start true})
          (nodes (:children props))))
    (let [[opts children] (normalize-component-args args)
          {:keys [class attrs]} (split-opts opts)]
      (el :div
          {:class (class-names "flex flex-wrap items-center gap-toolbar" class)}
          (merge-attrs attrs {:data-toolbar-start true})
          children))))

(defn toolbar-center
  "Centered toolbar region.

  Short form:
    (toolbar-center {:children [...]})

  Long form:
    (toolbar-center
      ...)"
  [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))]
      (el :div
          {:class (class-names "flex flex-wrap items-center justify-center gap-toolbar flex-1 min-w-0" class)}
          (merge-attrs attrs {:data-toolbar-center true})
          (nodes (:children props))))
    (let [[opts children] (normalize-component-args args)
          {:keys [class attrs]} (split-opts opts)]
      (el :div
          {:class (class-names "flex flex-wrap items-center justify-center gap-toolbar flex-1 min-w-0" class)}
          (merge-attrs attrs {:data-toolbar-center true})
          children))))

(defn toolbar-end
  "Trailing toolbar region.

  Short form:
    (toolbar-end {:children [...]})

  Long form:
    (toolbar-end
      ...)"
  [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))]
      (el :div
          {:class (class-names "flex flex-wrap items-center justify-end gap-toolbar" class)}
          (merge-attrs attrs {:data-toolbar-end true})
          (nodes (:children props))))
    (let [[opts children] (normalize-component-args args)
          {:keys [class attrs]} (split-opts opts)]
      (el :div
          {:class (class-names "flex flex-wrap items-center justify-end gap-toolbar" class)}
          (merge-attrs attrs {:data-toolbar-end true})
          children))))

(defn toolbar-spacer
  "Flexible spacer for toolbar layouts.

  Long form:
    (toolbar-spacer)

  Short form:
    (toolbar-spacer {:class ... :attrs ...})"
  [& args]
  (let [[opts _children] (normalize-component-args args)
        {:keys [class attrs]} (split-opts opts)]
    (el :div
        {:class (class-names "flex-1" class)}
        (merge-attrs attrs {:data-toolbar-spacer true})
        [])))

(defn toolbar
  "Toolbar layout for local controls and actions.

  Short form:
    (toolbar
      {:start [...]
       :center [...]
       :end [...]
       :children [...]})

  Long form:
    (toolbar {}
      (toolbar-start ...)
      (toolbar-center ...)
      (toolbar-spacer)
      (toolbar-end ...))

  Notes:
    - Toolbars are local control regions, not app-level chrome.
    - They are layout-only and do not add sticky or interactive behavior.
    - Center and spacer are both optional."
  [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))
          {:keys [start center end children]} props]
      (el :div
          {:class (class-names "toolbar-theme justify-between" class)}
          (merge-attrs attrs {:data-toolbar true})
          [(when start
             (apply toolbar-start {} (nodes start)))
           (when center
             (apply toolbar-center {} (nodes center)))
           (when end
             (apply toolbar-end {} (nodes end)))
           (nodes children)]))
    (let [[opts children] (normalize-component-args args)
          {:keys [class attrs]} (split-opts opts)]
      (el :div
          {:class (class-names "toolbar-theme justify-between" class)}
          (merge-attrs attrs {:data-toolbar true})
          children))))

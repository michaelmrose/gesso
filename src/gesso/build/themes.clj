(ns gesso.build.themes
  (:require
   [babashka.fs :as fs]
   [clojure.java.io :as io]
   [clojure.string :as str]))

(def ^:private axis-specs
  [{:axis :color-theme
    :dir "color-theme"
    :attr "data-color-theme"
    :requires-dark? true}

   {:axis :density
    :dir "density"
    :attr "data-density"
    :requires-dark? false}

   {:axis :typography
    :dir "typography"
    :attr "data-typography"
    :requires-dark? false}

   {:axis :shape
    :dir "shape"
    :attr "data-shape"
    :requires-dark? false}])

(def ^:private utilities-css-resource
  "gesso/theme-utilities.css")

(def ^:private default-component-css-dirs
  ["src/gesso/components"])

(defn- preset-name-from-path
  [path]
  (-> path fs/file-name str (str/replace #"\.css$" "")))

(defn- component-name-from-path
  [path]
  (-> path fs/parent fs/file-name str))

(defn- component-css-path
  [component-dir]
  (let [component-name (-> component-dir fs/file-name str)]
    (fs/path component-dir (str component-name ".css"))))

(defn- find-matching-brace
  [s open-idx]
  (loop [i (inc open-idx)
         depth 1]
    (when (< i (count s))
      (case (.charAt ^String s i)
        \{ (recur (inc i) (inc depth))
        \} (if (= depth 1)
             i
             (recur (inc i) (dec depth)))
        (recur (inc i) depth)))))

(defn- extract-block
  [css selector]
  (when-let [idx (str/index-of css selector)]
    (when-let [open-idx (str/index-of css "{" idx)]
      (when-let [close-idx (find-matching-brace css open-idx)]
        (subs css idx (inc close-idx))))))

(defn- strip-theme-inline-blocks
  [css]
  (loop [css css]
    (if-let [block (extract-block css "@theme inline")]
      (recur (str/replace css block ""))
      css)))

(defn- rewrite-root-block
  [attr preset-name block]
  (str/replace block
               #":root\s*\{"
               (str "html[" attr "~=\"" preset-name "\"] {")))

(defn- rewrite-dark-block
  [attr preset-name block]
  (str/replace block
               #"\.dark\s*\{"
               (str "html.dark[" attr "~=\"" preset-name "\"] {")))

(defn- preset-css
  [{:keys [axis attr requires-dark?]} path]
  (let [preset-name (preset-name-from-path path)
        css         (-> (slurp (str path))
                        strip-theme-inline-blocks)
        root-block  (extract-block css ":root")
        dark-block  (extract-block css ".dark")]
    (when-not root-block
      (throw (ex-info "Missing :root block"
                      {:axis axis
                       :file (str path)})))
    (when (and requires-dark? (nil? dark-block))
      (throw (ex-info "Missing .dark block"
                      {:axis axis
                       :file (str path)})))
    (str "/* " (name axis) ": " preset-name " */\n"
         (rewrite-root-block attr preset-name root-block)
         (when dark-block
           (str "\n\n"
                (rewrite-dark-block attr preset-name dark-block)))
         "\n")))

(defn- css-files-in-dir
  [dir]
  (if (fs/exists? dir)
    (->> (fs/list-dir dir)
         (filter #(and (fs/regular-file? %)
                       (str/ends-with? (str %) ".css")))
         (sort-by str))
    []))

(defn- discover-presets
  [input-dir]
  (mapcat
   (fn [{:keys [dir] :as spec}]
     (let [preset-dir (fs/path input-dir dir)]
       (map (fn [path] {:spec spec :path path})
            (css-files-in-dir preset-dir))))
   axis-specs))

(defn- utilities-css
  []
  (if-let [res (io/resource utilities-css-resource)]
    (slurp res)
    (throw (ex-info "Theme utilities CSS resource does not exist"
                    {:resource utilities-css-resource}))))

(defn- coerce-dir-list
  [x]
  (cond
    (nil? x)
    []

    (string? x)
    [x]

    (sequential? x)
    (vec x)

    :else
    [(str x)]))

(defn- normalize-component-css-dirs
  [{:keys [component-css-dir component-css-dirs]}]
  (let [plural-dirs  (coerce-dir-list component-css-dirs)
        singular-dir (coerce-dir-list component-css-dir)]
    (cond
      (seq plural-dirs)
      plural-dirs

      (seq singular-dir)
      singular-dir

      :else
      default-component-css-dirs)))

(defn- discover-component-css-files-in-dir
  [component-css-dir]
  (let [dir (fs/path component-css-dir)]
    (when-not (fs/exists? dir)
      (throw
       (ex-info "Component CSS root does not exist"
                {:component-css-dir (str dir)})))
    (when-not (fs/directory? dir)
      (throw
       (ex-info "Component CSS root is not a directory"
                {:component-css-dir (str dir)})))
    (->> (fs/list-dir dir)
         (filter fs/directory?)
         (map component-css-path)
         (filter #(and (fs/exists? %)
                       (fs/regular-file? %)))
         (sort-by str))))

(defn- discover-component-css-files
  [component-css-dirs]
  (->> component-css-dirs
       (mapcat discover-component-css-files-in-dir)
       distinct
       (sort-by str)))

(defn- component-css
  [path]
  (let [component-name (component-name-from-path path)
        css            (slurp (str path))]
    (str "/* component: " component-name " */\n"
         css
         (when-not (str/ends-with? css "\n")
           "\n"))))

(defn build!
  "Reads raw preset CSS files from :input-dir and writes a generated CSS bundle
  to :output-file.

  Supported preset axes are discovered by directory:

    resources/themes/color-theme/*.css
    resources/themes/density/*.css
    resources/themes/typography/*.css
    resources/themes/shape/*.css

  Color-theme files require both :root and .dark blocks.
  Other preset files require :root and may optionally include .dark.

  The generated bundle includes, in order:

    1. Generated preset CSS
    2. Shared utility CSS from gesso/theme-utilities.css
    3. Optional per-component CSS discovered from component CSS roots

  Component CSS roots are expected to contain one subdirectory per component.
  Each component subdirectory may include a CSS file whose basename matches the
  component directory name:

    src/gesso/components/button/button.css
    src/gesso/components/card/card.css

  Downstream apps can pass additional component CSS roots:

    :component-css-dirs [\"src/my_app/components\"
                         \"src/my_app/site/components\"
                         \"src/components\"]

  Component CSS discovery is silent:
    - if a component CSS root does not exist an exception is thrown
    - if a component directory has no matching component_name.css, nothing is added

  Options:

    :input-dir           defaults to resources/themes
    :output-file         defaults to resources/public/gesso/themes.css
    :component-css-dir   backward-compatible singular component root
    :component-css-dirs  preferred plural component roots

  Defaults are intended for the gesso repo itself:

    :component-css-dirs [\"src/gesso/components\"]"
  ([] (build! {}))
  ([{:keys [input-dir output-file] :as opts
     :or   {input-dir "resources/themes"
            output-file "resources/public/gesso/themes.css"}}]
   (let [input-dir          (fs/path input-dir)
         output-file        (fs/path output-file)
         component-css-dirs (normalize-component-css-dirs opts)]
     (when-not (fs/exists? input-dir)
       (throw (ex-info "Theme input directory does not exist"
                       {:input-dir (str input-dir)})))
     (let [presets             (discover-presets input-dir)
           component-css-files (discover-component-css-files component-css-dirs)]
       (when (empty? presets)
         (throw (ex-info "No preset CSS files found"
                         {:input-dir (str input-dir)})))
       (fs/create-dirs (fs/parent output-file))
       (spit (str output-file)
             (str
              (str/join
               "\n\n"
               (concat
                (map (fn [{:keys [spec path]}]
                       (preset-css spec path))
                     presets)
                [(utilities-css)]
                (map component-css component-css-files)))
              "\n"))
       (println "Wrote" (str output-file))
       (doseq [{:keys [spec path]} presets]
         (println " -" (name (:axis spec)) (preset-name-from-path path)))
       (doseq [dir component-css-dirs]
         (println " -" "component-css-root" dir))
       (doseq [path component-css-files]
         (println " -" "component-css" (component-name-from-path path)))))))

(defn -main
  [& _]
  (build!))

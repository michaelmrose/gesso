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

(defn- preset-name-from-path
  [path]
  (-> path fs/file-name str (str/replace #"\.css$" "")))

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

  The generated bundle also includes a small semantic utility layer for
  consuming theme variables directly in markup.

  Defaults are intended for the gesso repo itself:
    input-dir   => resources/themes
    output-file => resources/public/gesso/themes.css

  Downstream apps can call this with a different output file, e.g.
    resources/public/gesso/app-themes.css"
  ([] (build! {}))
  ([{:keys [input-dir output-file]
     :or   {input-dir "resources/themes"
            output-file "resources/public/gesso/themes.css"}}]
   (let [input-dir (fs/path input-dir)]
     (when-not (fs/exists? input-dir)
       (throw (ex-info "Theme input directory does not exist"
                       {:input-dir (str input-dir)})))
     (let [presets (discover-presets input-dir)]
       (when (empty? presets)
         (throw (ex-info "No preset CSS files found"
                         {:input-dir (str input-dir)})))
       (fs/create-dirs (fs/parent output-file))
       (spit output-file
             (str (str/join "\n\n"
                            (concat
                             (map (fn [{:keys [spec path]}]
                                    (preset-css spec path))
                                  presets)
                             [(utilities-css)]))
                  "\n"))
       (println "Wrote" output-file)
       (doseq [{:keys [spec path]} presets]
         (println " -" (name (:axis spec)) (preset-name-from-path path)))))))

(defn -main
  [& _]
  (build!))

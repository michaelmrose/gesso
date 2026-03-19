(ns gesso.build.themes
  (:require
   [babashka.fs :as fs]
   [clojure.string :as str]))

(defn- theme-name-from-path
  [path]
  (-> path fs/file-name str (str/replace #"\.css$" "")))

(defn- extract-block
  [css selector]
  (let [idx (str/index-of css selector)]
    (when (some? idx)
      (let [open-idx (str/index-of css "{" idx)]
        (when (some? open-idx)
          (loop [i (inc open-idx)
                 depth 1]
            (when (< i (count css))
              (let [ch (.charAt ^String css i)]
                (cond
                  (= ch \{)
                  (recur (inc i) (inc depth))

                  (= ch \})
                  (if (= depth 1)
                    (subs css idx (inc i))
                    (recur (inc i) (dec depth)))

                  :else
                  (recur (inc i) depth))))))))))

(defn- strip-theme-inline-block
  [css]
  (if-let [block (extract-block css "@theme inline")]
    (str/replace css block "")
    css))

(defn- rewrite-root-block
  [theme-name block]
  (str/replace block
               #":root\s*\{"
               (str "html[data-theme=\"" theme-name "\"] {")))

(defn- rewrite-dark-block
  [theme-name block]
  (str/replace block
               #"\.dark\s*\{"
               (str "html.dark[data-theme=\"" theme-name "\"] {")))

(defn- theme-css
  [path]
  (let [theme-name (theme-name-from-path path)
        css        (-> (slurp (str path))
                       strip-theme-inline-block)
        root-block (extract-block css ":root")
        dark-block (extract-block css ".dark")]
    (when-not root-block
      (throw (ex-info "Missing :root block" {:file (str path)})))
    (when-not dark-block
      (throw (ex-info "Missing .dark block" {:file (str path)})))
    (str "/* " theme-name " */\n"
         (rewrite-root-block theme-name root-block)
         "\n\n"
         (rewrite-dark-block theme-name dark-block)
         "\n")))

(defn build!
  "Reads raw theme files from :input-dir and writes a generated CSS bundle to :output-file.

  Defaults are intended for the gesso repo itself:
    input-dir   => resources/themes
    output-file => resources/public/gesso/themes.css

  Downstream apps can call this with a different output file, e.g.
    resources/public/gesso/app-themes.css"
  ([] (build! {}))
  ([{:keys [input-dir output-file]
     :or   {input-dir "resources/themes"
            output-file "resources/public/gesso/themes.css"}}]
   (let [theme-dir (fs/path input-dir)]
     (when-not (fs/exists? theme-dir)
       (throw (ex-info "Theme input directory does not exist"
                       {:input-dir input-dir})))
     (let [files (->> (fs/list-dir theme-dir)
                      (filter #(and (fs/regular-file? %)
                                    (str/ends-with? (str %) ".css")))
                      (sort-by str))]
       (when (empty? files)
         (throw (ex-info "No theme files found"
                         {:input-dir input-dir})))
       (fs/create-dirs (fs/parent output-file))
       (spit output-file
             (str (str/join "\n\n" (map theme-css files))
                  "\n"))
       (println "Wrote" output-file)
       (doseq [f files]
         (println " -" (theme-name-from-path f)))))))

(defn -main
  [& _]
  (build!))

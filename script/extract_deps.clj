#!/usr/bin/env bb

(ns gesso.script.extract-deps
  "Create a portable archive of the resolved Gesso development/test classpath.

   The archive is intended for reproducing Gesso's JVM/CLJS dependency
   environment somewhere that does not share the developer's ~/.m2 or ~/.gitlibs.

   By default:

     bb script/extract_deps.clj

   resolves :dev:test and writes:

     gesso-deps.tar.gz

   Useful options:

     --output PATH       archive destination
     --aliases ALIASES   tools.deps aliases, default :dev:test
     --clojure PATH      Clojure CLI executable, default $CLOJURE or clojure
     --strict            fail if any classpath entry reported by tools.deps is missing
     --help

   The archive deliberately contains dependencies and metadata, not Gesso source.
   Project-local classpath entries such as src/, resources/, and test/ remain as
   relative entries in metadata/relocated-classpath.txt."
  (:require
   [babashka.fs :as fs]
   [clojure.string :as str])
  (:import
   [java.io File]
   [java.nio.file Files Path StandardCopyOption]
   [java.time Instant]
   [java.util UUID]))

(def ^:private default-aliases
  ":dev:test")

(def ^:private default-output
  "gesso-deps.tar.gz")

(defn- usage
  []
  (str
   "Usage: bb script/extract_deps.clj [options]\n"
   "\n"
   "Options:\n"
   "  --output PATH       Archive destination (default: " default-output ")\n"
   "  --aliases ALIASES   tools.deps aliases (default: " default-aliases ")\n"
   "  --clojure PATH      Clojure CLI executable (default: $CLOJURE or clojure)\n"
   "  --strict            Fail if tools.deps reports any missing classpath entry\n"
   "  --help              Show this help\n"))

(defn- fail!
  ([message]
   (throw
    (ex-info
     message
     {:error/type :gesso.extract-deps/error})))
  ([message data]
   (throw
    (ex-info
     message
     (merge
      {:error/type :gesso.extract-deps/error}
      data)))))

(defn- parse-args
  [args]
  (loop [remaining args
         opts {:aliases default-aliases
               :output default-output
               :clojure (or (System/getenv "CLOJURE")
                            "clojure")
               :strict? false
               :help? false}]
    (if-let [arg (first remaining)]
      (case arg
        "--help"
        (recur (next remaining)
               (assoc opts :help? true))

        "--strict"
        (recur (next remaining)
               (assoc opts :strict? true))

        "--output"
        (if-let [value (second remaining)]
          (recur (nnext remaining)
                 (assoc opts :output value))
          (fail! "--output requires a path."))

        "--aliases"
        (if-let [value (second remaining)]
          (recur (nnext remaining)
                 (assoc opts :aliases value))
          (fail! "--aliases requires an alias string such as :dev:test."))

        "--clojure"
        (if-let [value (second remaining)]
          (recur (nnext remaining)
                 (assoc opts :clojure value))
          (fail! "--clojure requires an executable path."))

        (fail!
         (str "Unknown argument: " arg)
         {:argument arg}))
      opts)))

(defn- process-result
  [cwd command]
  (let [builder (ProcessBuilder. ^java.util.List (mapv str command))
        _ (.directory builder (.toFile ^Path cwd))
        process (.start builder)
        stdout-future (future (slurp (.getInputStream process)))
        stderr-future (future (slurp (.getErrorStream process)))
        exit (.waitFor process)]
    {:command (mapv str command)
     :exit exit
     :out @stdout-future
     :err @stderr-future}))

(defn- command!
  [cwd command]
  (let [{:keys [exit out err] :as result}
        (process-result cwd command)]
    (when-not (zero? exit)
      (fail!
       (str "Command failed with exit status " exit ": "
            (str/join " " command)
            (when-not (str/blank? err)
              (str "\n\n" (str/trim err))))
       {:command (:command result)
        :exit exit
        :stderr err}))
    out))

(defn- repository-root
  []
  (let [cwd (fs/normalize (fs/absolutize (fs/cwd)))
        result
        (try
          (process-result cwd ["git" "rev-parse" "--show-toplevel"])
          (catch Throwable _
            nil))]
    (cond
      (and result
           (zero? (:exit result))
           (not (str/blank? (:out result))))
      (fs/normalize
       (fs/absolutize
        (str/trim (:out result))))

      (fs/exists? (fs/path cwd "deps.edn"))
      cwd

      :else
      (fail!
       "Cannot locate the Gesso repository root. Run this script inside the repository."
       {:cwd (str cwd)}))))

(defn- path-inside?
  [parent child]
  (.startsWith
   ^Path (fs/normalize (fs/absolutize child))
   ^Path (fs/normalize (fs/absolutize parent))))

(defn- resolve-classpath-entry
  [repo-root entry]
  (let [path (fs/path entry)]
    (if (.isAbsolute ^Path path)
      (fs/normalize path)
      (fs/normalize
       (fs/path repo-root path)))))

(defn- classify-external
  [path]
  (let [value (-> (str path)
                  (str/replace "\\" "/"))]
    (cond
      (str/includes? value "/.m2/repository/")
      "maven"

      (str/includes? value "/.gitlibs/libs/")
      "gitlib"

      :else
      "external")))

(defn- relative-project-entry
  [repo-root resolved]
  (str
   (fs/relativize
    (fs/normalize repo-root)
    (fs/normalize resolved))))

(defn- copy-file!
  [source destination]
  (fs/create-dirs (fs/parent destination))
  (Files/copy
   ^Path source
   ^Path destination
   (into-array
    java.nio.file.CopyOption
    [StandardCopyOption/REPLACE_EXISTING]))
  destination)

(defn- copy-entry!
  [source destination-root]
  (cond
    (fs/regular-file? source)
    (let [destination
          (fs/path destination-root
                   (str (fs/file-name source)))]
      (copy-file! source destination)
      destination)

    (fs/directory? source)
    (let [destination
          (fs/path destination-root "content")]
      (fs/copy-tree source destination
                    {:replace-existing true})
      destination)

    :else
    (fail!
     "Classpath entry exists but is neither a regular file nor a directory."
     {:entry (str source)})))

(defn- write-lines!
  [path lines]
  (fs/create-dirs (fs/parent path))
  (spit (str path)
        (if (seq lines)
          (str (str/join "\n" lines) "\n")
          "")))

(defn- copy-metadata-file!
  [repo-root metadata-root filename]
  (let [source (fs/path repo-root filename)]
    (when (fs/regular-file? source)
      (copy-file!
       source
       (fs/path metadata-root filename)))))

(defn- clojure-classpath
  [repo-root {:keys [clojure aliases]}]
  (let [alias-arg
        (when-not (str/blank? aliases)
          (str "-A" aliases))
        command
        (cond-> [clojure "-Spath"]
          alias-arg
          (conj alias-arg))]
    (let [classpath
          (str/trim
           (command! repo-root command))]
      (when (str/blank? classpath)
        (fail!
         "Clojure CLI returned an empty classpath."
         {:command command}))
      classpath)))

(defn- clojure-description
  [repo-root clojure]
  (try
    (str/trim
     (command! repo-root [clojure "-Sdescribe"]))
    (catch Throwable error
      (str "unavailable: " (.getMessage error)))))

(defn- archive!
  [repo-root staging output]
  (fs/create-dirs (fs/parent output))
  (when (fs/exists? output)
    (fs/delete output))
  (command!
   repo-root
   ["tar"
    "-C" (str staging)
    "-czf" (str output)
    "."])
  output)

(defn- package-classpath!
  [repo-root staging classpath]
  (let [metadata-root (fs/path staging "metadata")
        deps-root (fs/path staging "deps")
        raw-entries
        (->> (str/split classpath
                        (re-pattern
                         (java.util.regex.Pattern/quote
                          File/pathSeparator)))
             (remove str/blank?)
             vec)]
    (fs/create-dirs metadata-root)
    (fs/create-dirs deps-root)

    (loop [remaining raw-entries
           dependency-index 1
           project-count 0
           copied []
           missing []
           relocated []]
      (if-let [entry (first remaining)]
        (let [resolved
              (resolve-classpath-entry repo-root entry)]
          (cond
            (path-inside? repo-root resolved)
            (recur
             (next remaining)
             dependency-index
             (inc project-count)
             copied
             missing
             (conj relocated
                   (relative-project-entry
                    repo-root
                    resolved)))

            (not (fs/exists? resolved))
            (recur
             (next remaining)
             dependency-index
             project-count
             copied
             (conj missing
                   {:kind (classify-external resolved)
                    :source (str resolved)})
             relocated)

            :else
            (let [kind (classify-external resolved)
                  destination-root
                  (fs/path deps-root
                           (str dependency-index))
                  copied-path
                  (copy-entry!
                   resolved
                   destination-root)
                  bundled-relative
                  (str
                   (fs/relativize
                    staging
                    copied-path))]
              (recur
               (next remaining)
               (inc dependency-index)
               project-count
               (conj copied
                     {:index dependency-index
                      :kind kind
                      :source (str resolved)
                      :bundled bundled-relative})
               missing
               (conj relocated
                     bundled-relative)))))
        {:raw-count (count raw-entries)
         :project-count project-count
         :copied copied
         :missing missing
         :relocated relocated}))))

(defn- metadata!
  [repo-root staging {:keys [aliases clojure]} classpath packaged]
  (let [metadata-root (fs/path staging "metadata")
        copied (:copied packaged)
        missing (:missing packaged)
        project-name
        (str (fs/file-name repo-root))]

    (copy-metadata-file!
     repo-root metadata-root "deps.edn")
    (copy-metadata-file!
     repo-root metadata-root "bb.edn")

    (spit
     (str (fs/path metadata-root "classpath.txt"))
     (str classpath "\n"))

    (write-lines!
     (fs/path metadata-root "relocated-classpath.txt")
     [(str/join File/pathSeparator
                (:relocated packaged))])

    ;; Keep the historical three-column entries.tsv shape.
    (write-lines!
     (fs/path metadata-root "entries.tsv")
     (map
      (fn [{:keys [index kind source]}]
        (str index "\t" kind "\t" source))
      copied))

    ;; New explicit source -> bundled mapping for consumers that need to
    ;; reconstruct an absolute classpath after extraction.
    (write-lines!
     (fs/path metadata-root "bundle-entries.tsv")
     (map
      (fn [{:keys [index kind source bundled]}]
        (str index "\t"
             kind "\t"
             source "\t"
             bundled))
      copied))

    (write-lines!
     (fs/path metadata-root "missing.tsv")
     (map
      (fn [{:keys [kind source]}]
        (str kind "\t" source "\tmissing"))
      missing))

    (spit
     (str (fs/path metadata-root "clojure-sdescribe.txt"))
     (str (clojure-description
           repo-root clojure)
          "\n"))

    (spit
     (str (fs/path metadata-root "info.txt"))
     (str
      "archive-format=2\n"
      "project=" project-name "\n"
      "aliases=" aliases "\n"
      "generated-at=" (Instant/now) "\n"
      "dependency-count=" (count copied) "\n"
      "missing-classpath-entries=" (count missing) "\n"
      "project-classpath-entries-skipped="
      (:project-count packaged) "\n"
      "source-classpath-entry-count="
      (:raw-count packaged) "\n"
      "clojure=" clojure "\n"))

    packaged))

(defn- human-size
  [path]
  (let [bytes (Files/size ^Path path)]
    (cond
      (>= bytes (* 1024 1024 1024))
      (format "%.2f GiB"
              (/ (double bytes)
                 (* 1024.0 1024.0 1024.0)))

      (>= bytes (* 1024 1024))
      (format "%.2f MiB"
              (/ (double bytes)
                 (* 1024.0 1024.0)))

      (>= bytes 1024)
      (format "%.2f KiB"
              (/ (double bytes)
                 1024.0))

      :else
      (str bytes " B"))))

(defn- run!
  [{:keys [output strict?] :as opts}]
  (let [repo-root (repository-root)
        output-path
        (let [candidate (fs/path output)]
          (if (.isAbsolute ^Path candidate)
            (fs/normalize candidate)
            (fs/normalize
             (fs/path repo-root candidate))))
        staging
        (fs/path repo-root
                 "target"
                 (str "extract-deps-"
                      (UUID/randomUUID)))]
    (println "Resolving Gesso dependency classpath...")
    (println "  repository:" (str repo-root))
    (println "  aliases:   " (:aliases opts))
    (println "  clojure:   " (:clojure opts))
    (try
      (let [classpath
            (clojure-classpath repo-root opts)
            packaged
            (package-classpath!
             repo-root staging classpath)
            packaged
            (metadata!
             repo-root staging opts classpath packaged)]

        (when (and strict?
                   (seq (:missing packaged)))
          (fail!
           (str "Refusing to create archive: "
                (count (:missing packaged))
                " classpath "
                (if (= 1 (count (:missing packaged)))
                  "entry is"
                  "entries are")
                " missing.")
           {:missing (:missing packaged)}))

        (when (seq (:missing packaged))
          (binding [*out* *err*]
            (println
             "WARNING:"
             (count (:missing packaged))
             "classpath"
             (if (= 1 (count (:missing packaged)))
               "entry is"
               "entries are")
             "missing; recorded in metadata/missing.tsv.")))

        (println
         "Packaging"
         (count (:copied packaged))
         "dependency classpath entries...")
        (archive!
         repo-root staging output-path)

        (println "Dependency archive ready:")
        (println " " (str output-path))
        (println "  size:" (human-size output-path))
        (println
         "  project-local classpath entries:"
         (:project-count packaged))
        (println
         "  external entries copied:"
         (count (:copied packaged)))
        (println
         "  missing entries:"
         (count (:missing packaged)))
        output-path)
      (finally
        (when (fs/exists? staging)
          (fs/delete-tree staging))))))

(defn -main
  [& args]
  (let [{:keys [help?] :as opts}
        (parse-args args)]
    (if help?
      (print (usage))
      (run! opts))))

(try
  (apply -main *command-line-args*)
  (catch Throwable error
    (binding [*out* *err*]
      (println "extract-deps:" (.getMessage error))
      (when-let [data (ex-data error)]
        (when-let [missing (:missing data)]
          (doseq [{:keys [kind source]} missing]
            (println " " kind source)))))
    (System/exit 1)))

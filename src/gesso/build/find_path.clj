(ns gesso.build.find-path
  (:require [clojure.java.io :as io]
            [babashka.fs :as fs]))

(defn path-for
  [target]
  (some-> (io/resource target)
          .getPath
          fs/parent
          str))

(defn -main
  [& args]
  (if-some [target (first args)]
    (if-some [path (path-for target)]
      (println path)
      (do
        (binding [*out* *err*]
          (println "Could not resolve resource:" target))
        (System/exit 1)))
    (do
      (binding [*out* *err*]
        (println "Usage: bb -m gesso.build.find-path <classpath-resource>"))
      (System/exit 1))))

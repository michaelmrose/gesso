(ns gesso.live.acquisition
  "Portable static restrictions for the managed single-target acquisition profile.
   Physical owner identity, inherited DOM state and HTMX defaults are checked
   separately at the browser boundary."
  (:require [clojure.string :as str]))

(defn replacement-tokens?
  [tokens]
  (and (contains? #{"innerHTML" "outerHTML"} (first tokens))
       (every? #(or (contains? #{"swap:0" "swap:0ms" "swap:0s" "swap:0m"
                                 "transition:false" "ignoreTitle:true" "focus-scroll:false"} %)
                    (re-matches #"settle:[0-9]+(?:\.[0-9]+)?(?:ms|s|m)?" %))
               (rest tokens))))

(defn replacement-spec?
  [spec]
  (and (string? spec)
       (replacement-tokens? (str/split (str/trim spec) #"\s+"))))

(defn extension-declaration?
  [value]
  (and (or (nil? value) (string? value))
       (every? #{"" "sse" "ignore:sse"}
               (str/split (str/replace (or value "") " " "") #","))))

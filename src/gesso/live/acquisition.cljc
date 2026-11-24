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

(def oob-swap-attributes
  ["hx-swap-oob" "data-hx-swap-oob"])

(def sse-affordance-attributes
  ["sse-connect" "data-sse-connect" "sse-swap" "data-sse-swap"
   "sse-close" "data-sse-close" "data-gesso-live-invalidation"
   "hx-trigger" "data-hx-trigger"])

(defn direct-sse-trigger?
  [value]
  (and (string? value) (str/includes? value "sse:")))

(defn unadmitted-sse-affordance?
  "Replacement content cannot declare its own SSE connection, subscription,
   close behavior or invalidation-listener identity. Conservatively reject
   sse: anywhere in either trigger spelling, rather than reimplement HTMX's
   trigger parser. Stable framework-owned root/listener markup is separate."
  [attribute value]
  (and (some? value)
       (if (contains? #{"hx-trigger" "data-hx-trigger"} attribute)
         (direct-sse-trigger? (str value))
         (boolean (some #{attribute} sse-affordance-attributes)))))

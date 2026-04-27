(ns gesso.validation.scripts
  "Generates client-side Hyperscript gatekeepers for native HTML validation."
  (:require
   [clojure.string :as str]))

(def ^:private default-messages
  {:required  "Required."
   :minlength "Too short."
   :maxlength "Too long."
   :pattern   "Invalid format."
   :type      "Invalid format."
   :min       "Value is too small."
   :max       "Value is too large."
   :invalid   "Invalid input."})

(defn- escape-hs-string
  "Escape a string for insertion into a single-quoted Hyperscript string."
  [s]
  (-> (str s)
      (str/replace #"\\" (fn [_] "\\\\"))
      (str/replace #"'" (fn [_] "\\'"))
      (str/replace #"\r?\n" (fn [_] " "))))

(defn- message
  [messages k]
  (or (get messages k)
      ;; typeMismatch is common for <input type=\"email\">. Reuse the pattern
      ;; message when available because users do not care whether the browser
      ;; classified the failure as typeMismatch or patternMismatch.
      (when (= k :type)
        (get messages :pattern))
      (get default-messages k)
      (:invalid default-messages)))

(defn- escaped-message
  [messages k]
  (escape-hs-string (message messages k)))

(def ^:private gatekeeper-template
  "on input
  if my.checkValidity() is true
    add .hidden to #{{err-id}}
    put '' into #{{err-id}}
    call me.setAttribute('aria-invalid', 'false')
    trigger validateField on me
  else if my.dataset.touched == 'true'
    set msg to '{{invalid}}'
    if my.validity.valueMissing
      set msg to '{{required}}'
    else if my.validity.tooShort
      set msg to '{{minlength}}'
    else if my.validity.tooLong
      set msg to '{{maxlength}}'
    else if my.validity.typeMismatch
      set msg to '{{type}}'
    else if my.validity.patternMismatch
      set msg to '{{pattern}}'
    else if my.validity.rangeUnderflow
      set msg to '{{min}}'
    else if my.validity.rangeOverflow
      set msg to '{{max}}'
    end
    put msg into #{{err-id}}
    remove .hidden from #{{err-id}}
    call me.setAttribute('aria-invalid', 'true')
  end
end

on blur
  call me.setAttribute('data-touched', 'true')
  if my.checkValidity() is false
    set msg to '{{invalid}}'
    if my.validity.valueMissing
      set msg to '{{required}}'
    else if my.validity.tooShort
      set msg to '{{minlength}}'
    else if my.validity.tooLong
      set msg to '{{maxlength}}'
    else if my.validity.typeMismatch
      set msg to '{{type}}'
    else if my.validity.patternMismatch
      set msg to '{{pattern}}'
    else if my.validity.rangeUnderflow
      set msg to '{{min}}'
    else if my.validity.rangeOverflow
      set msg to '{{max}}'
    end
    put msg into #{{err-id}}
    remove .hidden from #{{err-id}}
    call me.setAttribute('aria-invalid', 'true')
  else
    add .hidden to #{{err-id}}
    put '' into #{{err-id}}
    call me.setAttribute('aria-invalid', 'false')
    trigger validateField on me
  end
end")

(defn- render-template
  [template replacements]
  (reduce-kv
   (fn [s k v]
     (str/replace s
                  (str "{{" (name k) "}}")
                  (str v)))
   template
   replacements))

(defn gatekeeper-script
  "Generate the Hyperscript attached to a field control.

   The script relies on native browser validity checks. It does not reimplement
   Malli. It maps ValidityState failures to messages and updates the field's
   error container."
  [messages err-id]
  (render-template
   gatekeeper-template
   {:err-id    (str err-id)
    :invalid   (escaped-message messages :invalid)
    :required  (escaped-message messages :required)
    :minlength (escaped-message messages :minlength)
    :maxlength (escaped-message messages :maxlength)
    :type      (escaped-message messages :type)
    :pattern   (escaped-message messages :pattern)
    :min       (escaped-message messages :min)
    :max       (escaped-message messages :max)}))

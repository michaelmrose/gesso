(ns gesso.validation.scripts
  "Generates the client-side Hyperscript gatekeepers.")

(defn- escape-quotes
  "Prevents custom Malli messages with apostrophes from breaking the Hyperscript string."
  [s]
  (when s (clojure.string/replace s #"'" "\\'")))

(defn gatekeeper-script
  "Generates the Hyperscript that runs on input/blur to check local HTML5 validity.
   Maps native ValidityState failures to custom Malli messages."
  [messages err-id]
  (let [req-msg (escape-quotes (:required messages "Required."))
        min-msg (escape-quotes (:minlength messages "Too short."))
        max-msg (escape-quotes (:maxlength messages "Too long."))
        pat-msg (escape-quotes (:pattern messages "Invalid format."))]
    (str
     "on input or blur "
     "  if my.checkValidity() is false "
     "    set msg to 'Invalid input' "
     "    if my.validity.valueMissing set msg to '" req-msg "' "
     "    else if my.validity.tooShort set msg to '" min-msg "' "
     "    else if my.validity.tooLong set msg to '" max-msg "' "
     "    else if my.validity.patternMismatch set msg to '" pat-msg "' "
     "    put msg into #" err-id " "
     "    remove .hidden from #" err-id " "
     "    set @aria-invalid on me to 'true' "
     "  else "
     "    add .hidden to #" err-id " "
     "    set @aria-invalid on me to 'false' "
     "    trigger validateField")))

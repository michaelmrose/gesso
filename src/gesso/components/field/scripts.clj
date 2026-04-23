(ns gesso.components.form.scripts
  "Client-side scripts for intercepting and managing form submissions.")

(defn submission-guard []
  (str
   ;; Hook into HTMX's validation lifecycle event
   "on htmx:validation:validate "
   ;; 1. Force every field to act like it was touched
   "  for el in <input, textarea, select/> in me "
   "    set el's @data-touched to 'true' "
   ;; 2. Trigger a blur to force the field's gatekeeper script to evaluate
   "    trigger blur on el "
   "  end "
   ;; 3. If any gatekeeper set an aria-invalid flag, silently kill the submission
   "  if <[aria-invalid='true']> in me length > 0 "
   "    halt the event "
   "  end"))

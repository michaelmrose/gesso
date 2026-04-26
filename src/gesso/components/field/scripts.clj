(ns gesso.components.field.scripts
  "Small field-level scripts.

   Validation-specific gatekeeper scripts live in gesso.validation.scripts.
   This namespace owns field UI recovery behavior that is independent of Malli
   and independent of local HTML5 validation rules.")

(defn ui-recovery-script
  "Return Hyperscript that clears a visible server-side field error when the
   user starts editing the control again.

   The script only clears errors that have been marked as server errors by the
   OOB validation response. This avoids fighting local browser validation errors
   produced by the gatekeeper script."
  [err-id]
  (when err-id
    (str
     "on keydown\n"
     "  set err to #" err-id "\n"
     "  if err and err.dataset.serverError == 'true'\n"
     "    add .hidden to err\n"
     "    put '' into err\n"
     "    set err.dataset.serverError to 'false'\n"
     "    set @aria-invalid on me to 'false'\n"
     "  end\n"
     "end")))

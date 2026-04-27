(ns gesso.components.field.scripts
  "Field-level scripts for stale server-error recovery.")

(defn ui-recovery-script
  "Return Hyperscript that clears a visible server-side field error when the
   user starts editing the control again.

   The script only clears errors marked as server errors by the OOB validation
   response. This avoids fighting local browser validation errors produced by
   the validation gatekeeper."
  [err-id]
  (when err-id
    (str
     "on keydown\n"
     "  set err to #" err-id "\n"
     "  if err and err.dataset.serverError == 'true'\n"
     "    add .hidden to err\n"
     "    put '' into err\n"
     "    set err.dataset.serverError to 'false'\n"
     "    call me.setAttribute('aria-invalid', 'false')\n"
     "  end\n"
     "end")))

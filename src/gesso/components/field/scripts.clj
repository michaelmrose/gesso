(ns gesso.components.field.scripts
  "Field-level scripts for server-error reveal and stale-error recovery.")

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

(defn server-error-reveal-script
  "Reveal only an installed validation marker loaded directly into this slot.
   HTMX innerHTML OOB swaps schedule htmx:load on inserted children at settle;
   oobAfterSwap is not reliable for this slot and can fire without installation.
   Resolve the control through the stable slot's data, not response code."
  []
  "on htmx:load
  set marker to my.querySelector(':scope > [data-validation-error-message]')
  if marker and event.target is marker
    remove .hidden from me
    set my.dataset.serverError to 'true'
    set field to document.getElementById(my.getAttribute('data-field-error-for'))
    if field and field.getAttribute('aria-errormessage') is my.id
      call field.setAttribute('aria-invalid', 'true')
    end
  end
end")

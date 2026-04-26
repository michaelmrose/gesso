(ns gesso.components.form.scripts
  "Form-level client scripts.

   Field validation gatekeepers live in gesso.validation.scripts.
   Field stale-error recovery scripts live in gesso.components.field.scripts.

   This namespace only owns form-level coordination, especially preventing
   submissions while descendant controls are marked invalid.")

(defn submission-guard
  "Return Hyperscript that prevents a form submit when any descendant control is
   currently marked aria-invalid=\"true\".

   The guard intentionally stays generic:
   - it does not know about Malli
   - it does not know about Gesso field internals
   - it does not render errors
   - it only asks controls to run their local blur validation, then checks the
     shared aria-invalid state signal

   This is attached to the form root when validation guarding is enabled."
  []
  "on htmx:validation:validate
  for el in <input/> in me
    set el's @data-touched to 'true'
    trigger blur on el
  end
  for el in <textarea/> in me
    set el's @data-touched to 'true'
    trigger blur on el
  end
  for el in <select/> in me
    set el's @data-touched to 'true'
    trigger blur on el
  end
  if <[aria-invalid='true']/> in me
    halt the event
  end")

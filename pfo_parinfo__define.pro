; +
; $Id: pfo_parinfo__define.pro,v 1.1 2003/12/18 23:45:47 jpmorgen Exp $

; pfo_parinfo__define.pro 

; This procedure makes use of the handy feature in IDL 5 that calls
; the procedure mystruct__define when mystruct is referenced.
; Unfortunately, if IDL calls this proceedure itself, it uses its own
; idea of what null values should be.  Call explicitly with an
; if you need to have a default structure with different initial values.

;; Add a pfo structure tag to the default parinfo structure defined
;; in mpfit_parinfo__define.pro.  Do this hierarchically to keep
;; things tidy.

pro pfo_parinfo__define, parinfo=parinfo
  
  ;; If no variables needed to be initialized in mpfit_parinfo, we
  ;; could use this statement:

  ;; parinfo = struct_append({mpfit_parinfo}, {pfo:{pfo_struct}})

  ;; or if we wanted to dump all of the pfo tags into the top level,
  ;; we would use struct_append({mpfit_parinfo}, {pfo_struct})

  ;; Get an initialized mpfit_parinfo structure
  mpfit_parinfo__define, parinfo=mpfit_parinfo
  pfo = {pfo : {pfo_struct}}
  parinfo = struct_append(mpfit_parinfo, pfo, name="pfo_parinfo")
end

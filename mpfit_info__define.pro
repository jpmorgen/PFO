; +
; $Id: pfo_init_parinfo.pro,v 1.1 2002/12/16 13:36:14 jpmorgen Exp jpmorgen $

; mpfit_info__define.pro 

; This procedure makes use of the handy feature in IDL 5 that calls
; the procedure mystruct__define when mystruct is referenced.
; Unfortunately, if IDL calls this proceedure itself, it uses its own
; idea of what null values should be.  Call explicitly with an
; if you need to have a default structure with different initial values.

;; Create a structure that contains all of the scaler input/output
;; parameters from an mpfit call.

pro mpfit_info__define,
  mpfit_info $
    = {mpfit_info, $
       nfev = long(0), $
       maxiter = long(0), $
       errmsg = '', $
       ftol = 0D, $
       xtol = 0D, $
       gtol = 0D, $
       niter = long(0), $
       status = long(0), $
       bestnorm = 0D}
  mpfir_info.nfev = -1
  mpfir_info.maxiter = -1
  mpfir_info.ftol = -1
  mpfir_info.xtol = -1
  mpfir_info.gtol = -1
  mpfir_info.niter = -1
  mpfir_info.status = 0
  mpfir_info.bestnorm = -1

end

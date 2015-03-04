; +
; $Id: mpfit_parinfo__define.pro,v 1.1 2003/12/19 00:02:56 jpmorgen Exp jpmorgen $

; mpfit_parinfo__define.pro 

; This procedure makes use of the handy feature in IDL 5 that calls
; the procedure mystruct__define when mystruct is referenced.
; Unfortunately, if IDL calls this proceedure itself, it uses its own
; idea of what null values should be.  Call explicitly with an
; if you need to have a default structure with different initial values.

;; Define the basic mpfit parinfo structure for use in the pfo
;; package.  This is basically Craig's original list plus "error" so
;; that results can be stored in here too.  Define the tags like IDL
;; does (nulls) and then assign defaults we need.  There are two
;; things that by including here, we are forcing ourseles to pay
;; attention to: value and mpprint.  The default on mpprint is easy:
;; 1.  Value is a little trickier.  If you really don't want to use
;; value, just redefine this structure without it and the pfo stuff
;; will live just fine.  Also, passing start_params will bypass the
;; use of .value.  

pro mpfit_parinfo__define, parinfo=parinfo
  parinfo = {mpfit_parinfo, $
             value	: 0D, $
             error	: 0D, $
             fixed	: 0, $
             limited	: [0,0], $ 
             limits	: [0.D,0.D], $
             parname	: '', $
             step	: 0D, $
             relstep	: 0D, $
             mpside	: 0, $
             mpmaxstep	: 0D, $
             tied	: '', $
             mpprint	: 0}
  parinfo.value = !values.d_nan
  parinfo.error = !values.d_nan
  parinfo.mpprint = 1
  
end

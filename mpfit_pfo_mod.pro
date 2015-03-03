; +
; $Id: mpfit_pfo_mod.pro,v 1.1 2015/03/03 22:04:51 jpmorgen Exp $

; mpfit_pfo_mod.pro 

;; This routine is a primitive of pfo_fcreate that allows command-line
;; arguments to easily fill the mpfit-specific tags in the pfo_parinfo
;; structure.  See also mpfit_parinfo__define.pro.  Once a function is
;; created, it is easier to modify it using the tags directly.

;; WARNING: an entire parinfo array must be passed in order for the
;; values to be permanently changed (see IDL manual "passing by
;; reference" section).  Use the idx positional parameter to pick out
;; individual parinfo records.

; -

;; This should be the last step in the chain of _EXTRA stuff, so IDL
;; will raise an error if a keyword is passed that is not in the
;; argument list.


pro mpfit_pfo_mod, parinfo, idx, value=value, error=error, $
                   fixed=fixed, limited=limited, limits=limits, $
                   parname=parname, step=step, mpside=mpside, $
                   mpmaxstep=mpmaxstep, tied=tied, mpprint=mpprint
  

  ;; If we generate an error, return to the calling code.  THe problem
  ;; _must_ be there, since this code is perfect ;-)
;;  ON_ERROR, 2

  ;; Pathological case, avoids lindgen(0) error
  n = N_elements(parinfo)
  if N_elements(parinfo) eq 0 then $
    return

  ;; Set up idx if none specified
  if N_elements(idx) eq 0 then $
    idx = lindgen(n)

  ;; Step through the individual tags.

  if keyword_set(value) then begin
     struct_array_assign, parinfo, idx, tagname='value', tagval=value
  endif
  if keyword_set(error) then begin
     struct_array_assign, parinfo, idx, tagname='error', tagval=error
  endif
  if keyword_set(fixed) then begin
     struct_array_assign, parinfo, idx, tagname='fixed', tagval=fixed
  endif
  if keyword_set(limited) then begin
     struct_array_assign, parinfo, idx, tagname='limited', tagval=limited
  endif
  if keyword_set(limits) then begin
     struct_array_assign, parinfo, idx, tagname='limits', tagval=limits
  endif
  if keyword_set(parname) then begin
     struct_array_assign, parinfo, idx, tagname='parname', tagval=parname
  endif
  if keyword_set(step) then begin
     struct_array_assign, parinfo, idx, tagname='step', tagval=step
  endif
  if keyword_set(mpside) then begin
     struct_array_assign, parinfo, idx, tagname='mpside', tagval=mpside
  endif
  if keyword_set(mpmaxstep) then begin
     struct_array_assign, parinfo, idx,tagname='mpmaxstep',tagval=mpmaxstep
  endif
  if keyword_set(tied) then begin
     struct_array_assign, parinfo, idx, tagname='tied', tagval=tied
  endif
  if keyword_set(mpprint) then begin
     struct_array_assign, parinfo, idx, tagname='mpprint', tagval=mpprint
  endif

end

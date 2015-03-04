; +
; $Id: pfo_funct_check.pro,v 1.2 2004/01/15 17:11:49 jpmorgen Exp jpmorgen $

; pfo_funct_check.pro 

;; This is a collection of error checking is common to all pfo_
;; functions.  It makes sure that Xin exists (default =
;; !pfo.ytemplate), params and parinfo are consistent (creates parinfo
;; if parinfo not specified) and in the case of simple functions with
;; a standard number of parameters (e.g. 4 for a Voigt), makes sure
;; there are just enough parameters for one instance of the function.
;; The return value is the indices of the parinfo records
;; corresponding to the function and npar, the number of parameters
;; found.  If no parameters are found, npar=0 and the return value is
;; -1 (like where)

; -

function pfo_funct_check, fn, Xin=Xin, params=params, parinfo=parinfo, $
  idx=idx, npar=npar

  ;; Errors will make more sense in the calling function
  ON_ERROR, 2

  init = {pfo_sysvar}

  ;; Handle some pathological cases
  npar = -1
  if N_elements(Xin) eq 0 then $
    Xin = [!pfo.ytemplate]
  if N_elements(params) eq 0 and N_elements(parinfo) eq 0 then $
    return, npar
  if N_elements(fn) eq 0 then $
    fn = !pfo.null

  ;; We can create parinfo on the fly if it was not specified.  This
  ;; is useful for printing, but maybe a little dangerous, since
  ;; unexpected behavior might result.
  if N_elements(parinfo) eq 0 then $
    parinfo = pfo_fcreate(fn)

  ;; Copy parinfo.values to params if params are not already specified
  ;; (e.g. printing parinfo records)
  if N_elements(params) eq 0 then $
    params = parinfo.value
     
  ;; This should catch typos where the index values of params and
  ;; parinfo were not the same variable.  Oh, but if I am using idx
  ;; all the time, this might be inconvenient.  Well, I'll know when
  ;; that time comes.
  if N_elements(params) ne N_elements(parinfo) then $
    message, 'ERROR: number of parameters and number of parinfo records do not match.  If you really want it that way (e.g. playing fast and loose with segments of parameter lists, feel free to remove this code.'

  ;; idx might not have been specified
  if N_elements(idx) eq 0 then $
    idx = lindgen(N_elements(parinfo))

  ;; Check to see if we got handed idx=-1
  if N_elements(idx) eq 1 then $
    if idx eq -1 then $
    return, idx

  ;; In case I decide to removed the restriction that params and
  ;; parinfo be the same length
  if N_elements(idx) gt N_elements(parinfo) or $
    N_elements(idx) gt N_elements(params) then $
    message, 'ERROR: too many idx values'

  ;; The null funciton is also used for printing, so don't use it to
  ;; search for indices.  Also, I am now using it in pfo_funct
  if fn eq !pfo.null then begin
     f_idx = where(parinfo[idx].pfo.status eq !pfo.active, npar)
  endif else begin
     ;; This is the meat of the code in the normal case
     f_idx = where(parinfo[idx].pfo.status eq !pfo.active and $
                   fix(parinfo[idx].pfo.ftype) eq fn, npar)
  endelse

  ;; return -1 a la IDL's where, if no active records of this function
  ;; are found
  if npar eq 0 then $
    return, -1

  ;; Unnest the indices
  f_idx = idx[f_idx]

  ;; Bail here if we don't know how many parameters we should have in
  ;; this function (e.g. poly)
  if !pfo.fnpars[fn] eq 0 then $
    return, f_idx

  ;; Check functions with definite numbers of parameters (e.g. Voigt)
  if npar mod !pfo.fnpars[fn] ne 0 then $
    message, 'ERROR: incorrect number of parameters for function type ' + !pfo.fnames[fn]
                    
  if npar / !pfo.fnpars[fn] ne 1 then $
    message, 'ERROR: only one '+!pfo.fnames[fn]+' function can be processed at a time.  Use pfo_funct to process multiple functions.'

  return, f_idx

end


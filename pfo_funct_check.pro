; +
; $Id: pfo_funct_check.pro,v 1.5 2010/12/31 19:37:42 jpmorgen Exp $

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
;; -1 (like where).  If the function is not properly defined, npar=-1

; -

function pfo_funct_check, fn, Xin=Xin, params=params, parinfo=parinfo, $
  idx=idx, npar=npar, any_status=any_status

  ;; Errors will make more sense in the calling function
  ;;ON_ERROR, 2

  init = {pfo_sysvar}
  init = {tok_sysvar}

  ;; Handle some pathological cases
  nparinfo = N_elements(parinfo)
  npar = -1
  if N_elements(params) eq 0 and nparinfo eq 0 then $
    return, npar
  if N_elements(Xin) eq 0 then $
    Xin = [!pfo.ytemplate]
  if N_elements(fn) eq 0 then $
    fn = !pfo.null

  ;; Check to see if we were called in widget mode.  If so, we want to
  ;; return all of the indices rather than those that are active.
  if size(parinfo, /type) eq !tok.pointer then begin
     ;; Check to see if we have a valid parinfo in our heap pointer.
     ;; This catches the case *parinfo = 'none'
     if size(*parinfo, /type) ne !tok.struct then $
       return, npar ;; This is -1 from above
     ;; Make it clear that we have a pointer and find the number of
     ;; parinfo elements
     pparinfo = parinfo
     nparinfo = N_elements(*pparinfo)
  endif

  ;; We can create parinfo on the fly if it was not specified.  This
  ;; is useful for printing, but maybe a little dangerous, since
  ;; unexpected behavior might result.  In the widget case, nparinfo
  ;; is never 0
  if nparinfo eq 0 then $
    parinfo = pfo_fcreate(fn)

  ;; We already checked for this for the pointer case, above.
  if NOT keyword_set(pparinfo) and size(parinfo, /type) ne !tok.struct then $
    return, npar ;; This is -1 from above

  ;; If we got here, we should have a valid parinfo or pparinfo.

  ;; Copy parinfo.values to params if params are not already specified
  ;; (e.g. printing parinfo records)
  if N_elements(params) eq 0 then begin
     if keyword_set(pparinfo) then $
       params = (*pparinfo).value $
     else $
       params = parinfo.value
  endif
     
  ;; This should catch typos where the index values of params and
  ;; parinfo were not the same variable.  Oh, but if I am using idx
  ;; all the time, this might be inconvenient.  Well, I'll know when
  ;; that time comes.
  if N_elements(params) ne nparinfo then $
    message, 'ERROR: number of parameters and number of parinfo records do not match.  If you really want it that way (e.g. playing fast and loose with segments of parameter lists, feel free to remove this code.'

  ;; idx might not have been specified
  if N_elements(idx) eq 0 then $
    idx = lindgen(nparinfo)

  ;; Check to see if we got handed idx=-1
  if N_elements(idx) eq 1 then $
    if idx eq -1 then $
    return, idx

  ;; In case I decide to removed the restriction that params and
  ;; parinfo be the same length
  if N_elements(idx) gt nparinfo or $
    N_elements(idx) gt N_elements(params) then $
    message, 'ERROR: too many idx values'

  ;; Here is the meat of our code.  We are going to return f_idx, the
  ;; indices into parinfo of the function(s) we are checking.  Start
  ;; with the default of everything.  This covers the case of fn eq
  ;; !pfo.null and /any_status (select all functions indiscriminately
  ;; -- useful for widget-based editing of function)
  f_idx = idx
  npar = N_elements(idx)
  ;; Now check to see if we want to only return indices to active
  ;; parinfo records (the default)
  if NOT keyword_set(any_status) then begin
     f_idx = where(parinfo[idx].pfo.status eq !pfo.active, npar)
     ;; return -1 a la IDL's where, if no active records of this function
     ;; are found
     if npar eq 0 then $
       return, f_idx
     ;; Unnest the indices
     f_idx = idx[f_idx]
  endif

  ;; In the case of fn eq !pfo.null, our work is done, since we use
  ;; that to troll for all active functions in pfo_funct.  In the case
  ;; that fn was specified, we have a little more work to do to make
  ;; sure everything is consistent.
  if fn ne !pfo.null then begin
     ;  Can't reuse f_idx too much and, don't want to clobber idx...
     g_idx = f_idx
     f_idx = where(fix(parinfo[g_idx].pfo.ftype) eq fn, npar)
     if npar eq 0 then $
       return, f_idx
     ;; Unnest the indices
     f_idx = g_idx[f_idx]
  endif
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


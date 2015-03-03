;+
; NAME: pfo_funct_check
;
; PURPOSE: provide common error checking functionality for PFO functions
;
; CATEGORY: PFO functions
;
; CALLING SEQUENCE: use_idx = pfo_funct_check(fname_or_fnum, Xin=Xin, params=params, parinfo=parinfo, idx=idx, npar=npar, any_status=any_status)
;
; DESCRIPTION: Performs common initialization and error checking for
; pfo functions.
;
; INPUTS: 

;	fname_or_fnum: string fname or integer fnum from the pfo_finfo
;	system.  If not specified, error checking is not done on any
;	specific function.  If specified

; OPTIONAL INPUTS:
;
; KEYWORD PARAMETERS:

; 	params

;
; OUTPUTS:
;
; OPTIONAL OUTPUTS:
;
; COMMON BLOCKS:  
;   Common blocks are ugly.  Consider using package-specific system
;   variables.
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;
; $Id: pfo_funct_check.pro,v 1.7 2015/03/03 21:27:44 jpmorgen Exp $
;
; $Log: pfo_funct_check.pro,v $
; Revision 1.7  2015/03/03 21:27:44  jpmorgen
; Summary: Obselete
;
;-
; +
; $Id: pfo_funct_check.pro,v 1.7 2015/03/03 21:27:44 jpmorgen Exp $

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

function pfo_funct_check, fname_or_fnum, Xin=Xin, params=params, parinfo=parinfo, idx=idx, npar=npar, $
  any_status=any_status, pfo_obj=pfo_obj

  init = {pfo_sysvar}
  init = {tok_sysvar}

  ;; Handle pfo_debug level.  CATCH errors if _not_ debugging
  if !pfo.debug le 0 then begin
     ;; Return to the calling routine with our error
     ON_ERROR, !tok.return
     CATCH, err
     if err ne 0 then begin
        CATCH, /CANCEL
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'USAGE: use_idx = pfo_funct_check(fname_or_fnum, Xin=Xin, params=params, parinfo=parinfo, idx=idx, npar=npar, any_status=any_status, pfo_obj=pfo_obj)'
     endif
  endif ;; not debugging

  ;; Handle some pathological cases
  N_parinfo = N_elements(parinfo)
  npar = -1
  if N_elements(params) eq 0 and N_parinfo eq 0 then $
    return, npar

  ;; Check to see if parinfo is a structure
  if size(parinfo, /type) ne !tok.struct then $
    return, npar ;; This is -1 from above

  ;; It is handy to initialize Xin because it is used as a positional
  ;; parameter a lot
  if N_elements(Xin) eq 0 and arg_present(Xin) then $
    Xin = [!pfo.ytemplate]

  ;; If we don't have a function specified, we are probably
  ;; being called from pfo_funct
  if N_elements(fname_or_fnum) eq 0 then begin
     fnum = !tok.nowhere
  endif else begin
     ;; Otherwise, get our fnum and fnpars
     fnum = pfo_fnum(fname_or_fnum, fnpars=fnpars, pfo_obj=pfo_obj)
  endelse

  ;; If we got here, we should have a valid parinfo

  ;; Copy parinfo.values to params if params are not already
  ;; specified.  We are going to handle all calculation, display, etc.
  ;; in terms of of params, since that is how the parameter optimizers
  ;; (e.g. MPFIT) think of the parameter values.  parinfo.value is
  ;; just the first guess at params
  if N_elements(params) eq 0 then $
    params = parinfo.value

  ;; Unfortunately MPFIT does not know about parinfo.pfo.status, so we
  ;; need to have one param per parinfo, even for inactive parameters
  ;; (see pfo_mode).
  if N_elements(params) ne N_parinfo then $
    message, 'ERROR: number of parameters and number of parinfo records do not match.  If you really want it that way (e.g. playing fast and loose with segments of parameter lists, feel free to remove this code).'

  ;; idx might not have been specified
  pfo_idx, parinfo, idx

  ;; Check to see if we got handed idx=!tok.nowhere
  if N_elements(idx) eq 1 then $
    if idx eq !tok.nowhere then $
    return, idx

;;  ;; In case I decide to removed the restriction that params and
;;  ;; parinfo be the same length
;;  if N_elements(idx) gt N_parinfo or $
;;    N_elements(idx) gt N_elements(params) then $
;;    message, 'ERROR: too many idx values'

  ;; Here is the meat of our code.  We are going to return f_idx, the
  ;; indices into parinfo of the function(s) we are checking.  If the
  ;; resulting indices have some problems, we will raise what errors
  ;; we can here, or return to the calling code and hope that the
  ;; calling code handles discrepancies properly.


  ;; Start with the default of everything.  This covers the case of
  ;; fnum eq !tok.nowhere and /any_status (select all functions
  ;; indiscriminately -- useful for widget-based editing of function)
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

  ;; In the case of fnum eq !tok.nowhere, our work is done, since we use
  ;; that to troll for all active functions in pfo_funct.
  if fnum eq !tok.nowhere then $
    return, f_idx


  ;; If we made it here, fnum was specified: we have a little more
  ;; work to do to make sure everything is consistent.

  ;;  Can't reuse f_idx too much and, don't want to clobber idx...
  g_idx = temporary(f_idx)
  f_idx = where(fix(parinfo[g_idx].pfo.ftype) eq fnum, npar)
  if npar eq 0 then $
    return, f_idx ;; which will be !tok.nowhere
  ;; Unnest the indices
  f_idx = g_idx[temporary(f_idx)]

  ;; Now check to see


  if !pfo.fnpars[fn] eq 0 then $
    return, f_idx

  ;; Check functions with definite numbers of parameters (e.g. Voigt)
  if npar mod !pfo.fnpars[fn] ne 0 then $
    message, 'ERROR: incorrect number of parameters for function type ' + !pfo.fnames[fn]
                    
  if npar / !pfo.fnpars[fn] ne 1 then $
    message, 'ERROR: only one '+!pfo.fnames[fn]+' function can be processed at a time.  Use pfo_funct to process multiple functions.'

  return, f_idx

end


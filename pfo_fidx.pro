;+
; NAME: pfo_fidx
;
; PURPOSE: finds a PFO function in a parinfo array
;
; CATEGORY: PFO functions
;
; CALLING SEQUENCE: fidx = pfo_fidx(parinfo, fname_or_fnum,
; idx=idx, npar=npar, nfunct=nfunct, status_mask=status_mask, pfo_obj=pfo_obj)

; DESCRIPTION: Searches parinfo array (optionally limited by idx and
; status_mask) for instance(s) of a function.  Either an fname or fnum
; can be specified.  If no function is specified, indices to all
; functions in the parinfo (subject to the idx and status_mask
; selections) are returned

; INPUTS: 

;	parinfo: parinfo array

;       fname_or_fnum: string fname or integer fnum from the pfo_finfo
;       system.  If not specified, indices to all functions are
;       returned (subject to the idx and status_mask selections)

; OPTIONAL INPUTS:
;
; KEYWORD PARAMETERS:

;	idx (optional): indices into parinfo array limiting search

;       npar (output): number of parameters describing function
;       fname_or_fnum.  npar=0 is returned if no parameters were
;       found.  npar=!tok.nowhere is returned if an parinfo is invalid

;       nfunct (output): number of instances of function found.
;       !tok.nowhere is returned if this number cannot be determined
;       (e.g. invalid parinfo, no functions of this type, or function
;       does not have a definitive number of arguments).  Note by
;       using idx = pfo_fidx(fname, parinfo=parinfo), parsed_idx =
;       pfo_funct(/INDICES, /terminate_idx, parinfo=parinfo, idx=idx),
;       and junk=pfo_parse_idx(parsed_idx, nsegments=nfunct) it is
;       possible to determine nfunct

;       status_mask (optional): bitmask allowing selection of
;       parameters based on their status in the pfo system (see
;       pfo_mode).  Default is
;       status_mask=!pfo.active. status_mask=!pfo.all_status returns
;       all currently defined status values

;	pfo_obj: pfo_obj that is storing the pfo_fstruct_array.  If
;	not defined, the PFO COMMON block is queried


;
; OUTPUTS:

;       indices into parinfo of selected function(s).  !tok.nowhere is
;       returned if no parameters were found.

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
; $Id: pfo_fidx.pro,v 1.4 2011/11/18 16:11:38 jpmorgen Exp $
;
; $Log: pfo_fidx.pro,v $
; Revision 1.4  2011/11/18 16:11:38  jpmorgen
; Minor change in default outputs
;
; Revision 1.3  2011/09/16 11:21:35  jpmorgen
; Fixed status_mask code.  Still not sure about best default value
;
; Revision 1.2  2011/09/01 22:28:59  jpmorgen
; Significant improvements to parinfo editing widget, created plotwin
; widget, added pfo_poly function.
;
; Revision 1.1  2011/08/01 19:18:16  jpmorgen
; Initial revision
;
;-

function pfo_fidx, parinfo, fname_or_fnum, idx=idx, $
                   npar=npar, nfunct=nfunct, status_mask=status_mask, $
                   pfo_obj=pfo_obj

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
        message, 'USAGE: f_idx = pfo_fidx(parinfo, fname_or_fnum, idx=idx, npar=npar, nfunct=nfunct, status_mask=status_mask, pfo_obj=pfo_obj)'
     endif
  endif ;; not debugging

  ;; Handle some pathological cases in a way that lets calling routine
  ;; deal with things gracefully (e.g. by checking for npar)
  N_parinfo = N_elements(parinfo)
  npar = -1
  nfunct = 0
  fnpars = fnpars
  if N_parinfo eq 0 then $
    return, npar

  ;; Check to see if parinfo is a structure
  if size(parinfo, /type) ne !tok.struct then $
    return, npar ;; This is -1 from above

  ;; If we got here, we should have a valid parinfo

  ;; Do the translation from fname to fnum, if necessary.  This
  ;; returns !tok.nowhere if fname_of_fnum is undefined
  fnum = pfo_fnum(fname_or_fnum, fnpars=fnpars, pfo_obj=pfo_obj)

  ;; Check to see if we got handed idx=!tok.nowhere
  if N_elements(idx) eq 1 then $
    if idx eq !tok.nowhere then $
    return, idx

  ;; idx might not have been specified
  pfo_idx, parinfo, idx

  ;; Here is the meat of our code.  We are going to return f_idx, the
  ;; indices into parinfo of the function(s) we are checking.  If the
  ;; resulting indices have some problems, we will raise what errors
  ;; we can here, or return to the calling code and hope that the
  ;; calling code handles discrepancies properly.

  ;; Our default bitmask for the mode (parinfo.pfo.status) of the
  ;; parameters we select
  if NOT keyword_set(status_mask) then $
    status_mask = !pfo.active

  ;; Apply our bitmask to the parinfo (optionally narrowed to idx)
  use_idx = where((parinfo[idx].pfo.status AND status_mask) gt 0, npar)
  ;; return -1 a la IDL's where, if no parameters matching our
  ;; bitmask are found
  if npar eq 0 then $
    return, use_idx

  ;; unwrap so we index into parinfo
  use_idx = idx[temporary(use_idx)]

  ;; In the case of fnum eq !tok.nowhere, our work is done.  nfunct=-1
  ;; at this point, since we don't know which functions we want
  if fnum eq !tok.nowhere then $
    return, use_idx

  ;; If we made it here, we have a specific function we are looking
  ;; for.  We have a little more work to do to make sure everything is
  ;; consistent.
  f_idx = where(floor(parinfo[use_idx].pfo.ftype) eq fnum, npar)
  if npar eq 0 then $
    return, f_idx ;; which will be !tok.nowhere
  ;; Unnest the indices
  f_idx = use_idx[temporary(f_idx)]

  ;; Now check to see if the numbers of parameters are consistent

  ;; Without going through the whole parsing excercise of pfo_funct,
  ;; we can't do much in the case where we don't know how many
  ;; parameters a function has.  Return sensible f_idx and npars but
  ;; nfunct=-1
  nfunct = !tok.nowhere
  if fnpars eq 0 then $
    return, f_idx

  ;; Check functions with definite numbers of parameters (e.g. Voigt)
  if npar mod fnpars ne 0 then $
    message, 'ERROR: incorrect number of parameters for function type ' + pfo_fname(fnum) + ': ' + strtrim(npar, 2)
  
  ;; If we made it here, we have a sensible (set of) function(s).  Let
  ;; the user check for npar and nfunct to make sure it makes sense

  nfunct = npar / fnpars

  return, f_idx

end


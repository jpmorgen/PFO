;+
; NAME: pfo_fnpars
;
; PURPOSE: returns number of parameters of a function in the PFO system
;
; CATEGORY: PFO functions
;
; CALLING SEQUENCE: fnpars = pfo_fnpars(fname_or_fnum[, fname=fname]
; [, fnum=fnum] [, fdescr=fdescr] [, pfo_obj=pfo_obj])
;
; DESCRIPTION: This function interfaces with the procedure pfo_finfo
; to enable convenient return of the number of parameters of a
; function in the PFO system.
;
; INPUTS: fname_or_fnum.  Conveniently allows lookup of fnpars via
; fname or fnum.
;
; OPTIONAL INPUTS:
;
; KEYWORD PARAMETERS: 
;	fname (output): function name
;	fnum (output): integer part of the tag parinfo.pfo.ftype
;	fdescr (output): function documentation

;	pfo_obj: pfo_obj that is storing the pfo_fstruct_array.  If
;	not defined, the PFO COMMON block is queried


;
; OUTPUTS:
;
; OPTIONAL OUTPUTS:
;
; COMMON BLOCKS:  
;   See pfo_finfo
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
; $Id: pfo_fnpars.pro,v 1.1 2011/08/01 19:18:16 jpmorgen Exp $
;
; $Log: pfo_fnpars.pro,v $
; Revision 1.1  2011/08/01 19:18:16  jpmorgen
; Initial revision
;
;-
function pfo_fnpars, $
  fname_or_fnum, $
  fname=fname, $
  fnum=fnum, $
  fdescr=fdescr, $
  pfo_obj=pfo_obj

  init = {pfo_sysvar}
  init = {tok_sysvar}
  if !pfo.debug le 0 then begin
     ;; Return to the calling routine with our error
     ON_ERROR, !tok.return
     CATCH, err
     if err ne 0 then begin
        CATCH, /CANCEL
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'USAGE: pfo_fnpars, fname_or_fnum=fname [, fname=fname] [, fnum=fnum] [,fnpars=fnpars] [, fdescr=fdescr] [, pfo_obj=pfo_obj]'
     endif
  endif ;; not debugging

  if size(/type, fname_or_fnum) eq !tok.string then $
    fname = fname_or_fnum $
  else $
    fnum = fname_or_fnum
  
  pfo_finfo, fname=fname, $
             fnum=fnum, $
             fnpars=fnpars, $
             fdescr=fdescr, $
             pfo_obj=pfo_obj

  return, fnpars

end


;+
; NAME: pfo_fnum
;
; PURPOSE: Returns the function number (integer part of
; parinfo.pfo.ftype) of a PFO-enabled function
;
; CATEGORY: PFO functions
;
; CALLING SEQUENCE: fnum = pfo_fnum(fname_or_fnum | parinfo=parinfo [, fname=fname][,
; fnpars=fnpars][, fdescr=fdescr][,
; pfo_fstruct_fdescr=pfo_fstruct_fdescr][, pfo_obj=pfo_obj])
;
; DESCRIPTION: This function allows easy conversion between a function
; name and its dynamically assigned fnum (the integer part of
; parinfo.pfo.ftype).  See pfo_finfo for more detail.
;
; INPUTS:
;
; OPTIONAL INPUTS: fname_or_fnum.  If a string, conversion is made to fnum
; using pfo_finfo.  If not a string, this input is assumed to be fnum,
; fname_or_fnum is set to the null string and control is passed to
; pfo_finfo.  If not specified, a 1-element parinfo array must be
; specified.
;
; KEYWORD PARAMETERS:

;	parinfo: a 1-element parinfo array from which to extract the
;		fnum (if fnum_or_fname not specified)
;	fnpars (output): number of parameters in the function (0 = unknown)
;	fdescr (output): function documentation
;	pfo_fstruct_descr (output): documentation of the structure
;		that contains fname, fnum, and fdescr.

;	pfo_obj: pfo_obj that is storing the pfo_fstruct_array.  If
;	not defined, the PFO COMMON block is queried

;
; OUTPUTS: dynamically assigned function number
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
; $Id: pfo_fnum.pro,v 1.1 2011/08/01 19:18:16 jpmorgen Exp $
;
; $Log: pfo_fnum.pro,v $
; Revision 1.1  2011/08/01 19:18:16  jpmorgen
; Initial revision
;
;-
function pfo_fnum, $
   fname_or_fnum, $ ;; function name or num, if conversion has occured elsewhere
   parinfo=parinfo, $
   fnpars=fnpars, $
   fdescr=fdescr, $
   pfo_fstruct_descr=pfo_fstruct_descr, $
   pfo_obj=pfo_obj ;; pfo_obj containing pfo_fstruct_array

  init = {pfo_sysvar}
  init = {tok_sysvar}
  if !pfo.debug le 0 then begin
     ;; Return to the calling routine with our error
     ON_ERROR, !tok.return
     CATCH, err
     if err ne 0 then begin
        CATCH, /CANCEL
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'USAGE: pfo_fnum, fname_or_fnum [,fnpars=fnpars][, fdescr=fdescr][, pfo_fstruct_fdescr=pfo_fstruct_fdescr][, pfo_obj=pfo_obj]'
     endif
  endif ;; not debugging

  ;; Sanity check input
  if N_elements(fname_or_fnum) + N_elements(parinfo) eq 0 then $
    return, !tok.nowhere

  ;; Differentiate between parinfo and fname_or_fnum cases
  if N_elements(parinfo) ne 0 then begin
     ;; Handle the parinfo case to get a number
     fnum = floor(parinfo.pfo.ftype)
  endif else begin
     ;; Check to see if we were handed a string.
     if size(/type, fname_or_fnum) ne !tok.string then begin
        ;; If fname_or_fnum is not a string, assume it is an fnum and let
        ;; pfo_finfo sort it out.
        fnum = fname_or_fnum
        fname_or_fnum = ''
     endif
  endelse

  pfo_finfo, fname=fname_or_fnum, $
             fnum=fnum, $
             fnpars=fnpars, $
             fdescr=fdescr, $
             pfo_fstruct_descr=pfo_fstruct_descr, $
             pfo_obj=pfo_obj

  return, fnum

end

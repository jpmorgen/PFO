;+
; NAME: pfo_fname

; PURPOSE: returns the function name  of a PFO-enabled function.

; CATEGORY: PFO functions
;
; CALLING SEQUENCE: fname = pfo_fname([fnum_or_fname | parinfo=parinfo] [, fnpars=fnpars][,
; fdescr=fdescr][, pfo_fstruct_fdescr=pfo_fstruct_fdescr][,
; pfo_obj=pfo_obj] [, callstack=callstack])
;
; DESCRIPTION:

; This function allows easy conversion between the dynamically
; assigned fnum (the integer part of parinfo.pfo.ftype) and its
; function name (fname).  See pfo_finfo for more detail about this
; syste.  If no positional parameters are given, the caller is assumed
; to be part of the function definition (e.g. <funct>__<method>) and
; the string <funct> is returned.

; INPUTS: 

; OPTIONAL INPUTS: fnum_or_fname.  If a string, conversion is made to fnum
; using pfo_finfo.  If not a string, this input is assumed to be fnum,
; fnum_or_fname is set to the null string and control is passed to
; pfo_finfo.  If not specified, a 1-element parinfo array must be specified.
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

;       callstack: in the case that fnum_or_fname and parinfo are not
;       specified, specifies how many levels up the callstack _from
;       the caller_ to go to grab the function definition name.  In
;       other words, a call from <funct>__<method> can use
;       pfo_fname().  But a call from something that method it calls
;       (e.f. pfo_calc_check) should use pfo_fname(callstack=1)

; OUTPUTS: function name
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
; $Id: pfo_fname.pro,v 1.2 2011/09/01 22:20:23 jpmorgen Exp $
;
; $Log: pfo_fname.pro,v $
; Revision 1.2  2011/09/01 22:20:23  jpmorgen
; Added cool feature to grab fname from calling routine
;
; Significant improvements to parinfo editing widget, created plotwin
; widget, added pfo_poly function.
;
; Revision 1.1  2011/08/01 19:18:16  jpmorgen
; Initial revision
;
;-
function pfo_fname, $
   fnum_or_fname, $ ;; function name or num, if conversion has occured elsewhere
   fnpars=fnpars, $
   fdescr=fdescr, $
   pfo_fstruct_descr=pfo_fstruct_descr, $
   parinfo=parinfo, $
   pfo_obj=pfo_obj, $ ;; pfo_obj containing pfo_fstruct_array
   callstack=callstack ;; how many levels to go back up the callstack to get the fname (when neither fnum_or_fname or parinfo are specified


  init = {pfo_sysvar}
  init = {tok_sysvar}
  if !pfo.debug le 0 then begin
     ;; Return to the calling routine with our error
     ON_ERROR, !tok.return
     CATCH, err
     if err ne 0 then begin
        CATCH, /CANCEL
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'USAGE: pfo_fname, fnum_or_fname[, parinfo=parinfo][,fnpars=fnpars][, fdescr=fdescr][, pfo_fstruct_fdescr=pfo_fstruct_fdescr][, pfo_obj=pfo_obj] [, callstack=callstack]'
     endif
  endif ;; not debugging

  ;; If we are not given any positional parameter or parinfo, assume
  ;; caller is a function "method."  The name of its function will be
  ;; whatever is in front of the '__'
  if N_elements(fnum_or_fname) + N_elements(parinfo) eq 0 then begin
     if N_elements(callstack) eq 0 then $
        callstack = 0
     help, calls=calls
     return, strmid(calls[callstack+1], 0, strpos(calls[callstack+1], '__'))
  endif ;; returning fname of caller

  ;; If we made it here, we need to have fnum_or_fname or parinfo

  if N_elements(fnum_or_fname) + N_elements(parinfo) ne 1 then $
     message, 'ERROR: specify either one fnum_or_fname or one element of a parinfo from which to extract fnum'

  ;; Differentiate between parinfo and fnum_or_fname cases
  if N_elements(parinfo) ne 0 then begin
     ;; Handle the parinfo case to get a number
     fnum = floor(parinfo.pfo.ftype)
  endif else begin
     ;; Check to see if fnum_or_fname is not a string.
     if size(/type, fnum_or_fname) ne !tok.string then begin
        ;; If fnum_or_fname is not a string, assume it is an fnum and let
        ;; pfo_finfo sort it out.
        fnum = floor(fnum_or_fname)
        fnum_or_fname = ''
     endif ;; fnum_or_fname is a number
  endelse 

  pfo_finfo, fname=fnum_or_fname, $
             fnum=fnum, $
             fnpars=fnpars, $
             fdescr=fdescr, $
             pfo_fstruct_descr=pfo_fstruct_descr, $
             pfo_obj=pfo_obj

  ;; Check to see if fname is valid in system
  if fnum eq !tok.nowhere then $
     message, 'ERROR: invalid or uninitialized fname: ' + fnum_or_fname

  return, fnum_or_fname

end

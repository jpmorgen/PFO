;+
; NAME: pfo_null__calc

; PURPOSE: This provides initialization for PFO function-handling
; routines, including pfo_parinfo_parse and the __calc "methods" of
; PFO-enabled functions.

; CATEGORY: PFO functions
;
; CALLING SEQUENCE: pfo_null__calc, Xin, params, parinfo=parinfo,
; idx=idx=fname=fname, pfo_obj=pfo_obj, _EXTRA=extra
;
; DESCRIPTION: This initilizes Xin, Yaxis, and params.  Optionally, provides
; a warning message for pfo_parinfo_parse when the fname parameter is
; specified (this is used in pfo_parinfo_parse to spot missing __calc
; methods).  Warning message for missing Xin is also provided
;
; INPUTS:
;
; OPTIONAL INPUTS:
;
; KEYWORD PARAMETERS:
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
; $Id: pfo_null__calc.pro,v 1.1 2011/08/01 19:18:16 jpmorgen Exp $
;
; $Log: pfo_null__calc.pro,v $
; Revision 1.1  2011/08/01 19:18:16  jpmorgen
; Initial revision
;
;-
function pfo_null__calc, $
  Xin, $ 	;; Input X axis in natural units 
  params, $ 	;; parameters (entire array).  This ends up getting ignored in pfo_null__calc
  parinfo=parinfo, $ ;; parinfo array (whole array).  This ends up getting ignored in pfo_null__calc
  idx=idx, $    ;; idx into parinfo over which to search for function.  This ends up getting ignored in pfo_null__calc
  fname=fname, $;; original fname of function in pfo_parinfo_parse -- for display of warning message
  pfo_obj=pfo_obj, $ ;; This ends up getting ignored in pfo_null__calc
  _REF_EXTRA=extra ;; Soak up any other keyword arguments and ignore them

  ;; Generic pfo system initialization
  init = {pfo_sysvar}
  init = {tok_sysvar}

  ;; Return to the calling routine with any errors
  ON_ERROR, !tok.return

  ;; Check for Xin
  if N_elements(Xin) eq 0 then $
    message, 'ERROR: Xin must be defined'
  ;; Done checking inputs

  ;; Check to see if we were called from pfo_parinfo_parse.  Do this
  ;; with /INFORMATIONAL, so that user can supress message with
  ;; pfo_quiet, 1
  if keyword_set(fname) then $
    message, /INFORMATIONAL, 'WARNING: calc "method" missing for function ' + fname + ', returnning 0'

  ;; Now that we have guaranteed a non-trivial Xin, make yaxis the
  ;; same dimensionality, but with the type of ytemplate.  
  return, Xin*0

end

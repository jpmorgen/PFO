;+
; NAME: pfo_frac
;
; PURPOSE: returns the fractional part of a real number
;
; CATEGORY: PFO additions to IDL basic libraries
;
; CALLING SEQUENCE: result = pfo_frac(number)
;
; DESCRIPTION:
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
; $Id: pfo_frac.pro,v 1.1 2011/08/01 19:18:16 jpmorgen Exp $
;
; $Log: pfo_frac.pro,v $
; Revision 1.1  2011/08/01 19:18:16  jpmorgen
; Initial revision
;
;-
function pfo_frac, number

  init = {pfo_sysvar}
  init = {tok_sysvar}

  if !pfo.debug le 0 then begin
     ;; Return to the calling routine with our error
     ON_ERROR, !tok.return
     CATCH, err
     if err ne 0 then begin
        CATCH, /CANCEL
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'USAGE: result = pfo_frac(number)'
     endif
  endif ;; not debugging
  
  ;; Complex numbers don't do the right thing in floor, at least in my
  ;; book.  They could be enabled by changing the lower bound from
  ;; !tok.complex to !tok.string, or I could write a pfo_floor that
  ;; did the floor on each part of the complex
  if !tok.complex le size(/type, number) and size(/type, number) le !tok.objref then $
    message, 'ERROR: incompatible type ' + strtrim(size(number, /tname))

  return, number - floor(number)

end

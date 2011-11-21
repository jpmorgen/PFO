;+
; NAME: exp10
;
; PURPOSE: Returns 10^x
;
; CATEGORY: PFO helper functions
;
; CALLING SEQUENCE: y = exp10(x)

; DESCRIPTION: This is an examble of a routine that enables "call
; function" to be use instead of "execute" when a simple
; calculation is needed.  IDL's execute command is disabled in
; the IDL VM.

; INPUTS: x, the exponent
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
; $Id: exp10.pro,v 1.1 2011/11/21 14:58:23 jpmorgen Exp $
;
; $Log: exp10.pro,v $
; Revision 1.1  2011/11/21 14:58:23  jpmorgen
; Initial revision
;
;-
function exp10, x
  ;; This returns a float in all cases except when x is double, in
  ;; which case it returns a double
  return, 10.^x
end

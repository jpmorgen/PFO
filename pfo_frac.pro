;+
; NAME: pfo_frac
;
; PURPOSE: returns the fractional part of a real number, optionally
; truncating or rounding the result
;
; CATEGORY: PFO additions to IDL basic libraries
;
; CALLING SEQUENCE: result = pfo_frac(number [,/round] [, mult=mult])

; DESCRIPTION: This routine helps to get around problems of roundoff
; error when handling the fractional part of a real number.  

; INPUTS: 
;   number: number (or vector) which will have the fractional part returned
;
; OPTIONAL INPUTS:
;
; KEYWORD PARAMETERS: 

;   round: in the absense of the mult keyword, the fractional part of
;   "number" is multiplied by the appropriate power of 10, rounded,
;   and divided again, to preserve maximum machine precision, while
;   avoiding rounding error.  The result is the fractional part of the
;   input value with the same type, but with at least one "0" above
;   the least significant digit.  This is useful for comparing the
;   fractional parts of two numbers that may have different decimal
;   parts.  Note that one decimal of precision may be lost if "number"
;   was less than 0 to begin with.

;   mult: multiplier of fractional part (usually 10^n).  If specified,
;   pfo_frac returns an integer corresponding to the first n digits of
;   the fraction).  If /round is also specified, the decimal part of
;   the integer rounded, otherwise, the decimal part is truncated.

; OUTPUTS: decimal part of number either in the same type as the input
; or the first alog10(mult) digits as an integer

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

; print, pfo_frac([1.123456789d, 1.299])
;       0.12345679      0.29900002
; print, pfo_frac(111.123456789d, /round) eq pfo_frac(2.123456789d, /round)
;   1
; print, pfo_frac([1.123456789d, 1.299], mult=100)
;           12          29
; print, pfo_frac([1.123456789d, 1.299], mult=100, /round)
;           12          30

;
; MODIFICATION HISTORY:
;
; $Id: pfo_frac.pro,v 1.2 2012/01/13 20:51:55 jpmorgen Exp $
;
; $Log: pfo_frac.pro,v $
; Revision 1.2  2012/01/13 20:51:55  jpmorgen
; Updated to add round and mult features
;
; Revision 1.1  2011/08/01 19:18:16  jpmorgen
; Initial revision
;
;-
function pfo_frac, $
   number, $
   mult=mult_in, $
   round=round

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
  
  type = size(/type, number) 

  ;; Complex numbers don't do the right thing in floor, at least in my
  ;; book.  They could be enabled by changing the lower bound from
  ;; !tok.complex to !tok.string, or I could write a pfo_floor that
  ;; did the floor on each part of the complex
  if !tok.complex le type and type le !tok.objref then $
    message, 'ERROR: incompatible type ' + strtrim(size(number, /tname))

  ;; Prepare our output.  We want it to be the same size and type as
  ;; the input
  retval = 0 * number

  ;; Check to see if we are too big for floor(number, /L64).  If so,
  ;; IDL raises an error.  For our purposes, we are not going to have
  ;; enough precision to have any fractional part, so return 0 in
  ;; those cases
  good_idx = where(abs(number) le 2d^62, count)
  if count eq 0 then $
     return, retval

  ;; Set the L64 flag to floor, if necessary
  junk = where(abs(number[good_idx]) gt 2d^30, count)
  L64 = count ne 0

  ;; Calculate our answer for the cases that are non-zero.  Making
  ;; this calculation induces roundoff errors.
  retval[good_idx] = number[good_idx] - floor(number[good_idx], L64=L64)

  ;; Get our machine precision information 
  machine = machar(double=(type eq !tok.double))

  ;; Prepare to use our mult keyword.
  if keyword_set(mult_in) then $
     mult = mult_in

  ;; Check to see if we have the round keyword.  If so, set our
  ;; multiplier to be the total number of significant digits in our
  ;; data type minus the number of digits in the original integer
  ;; part.  Make sure there is always at least one digit in the
  ;; mantissa we are trying to round.
  if keyword_set(round) and NOT keyword_set(mult) then begin
     ;; Figure out the maximum number of decimal digits of
     ;; significance we have in our mantissa.  This is basically 6 or
     ;; 15, depending on float or double.
     fdigits =  floor(-1*(alog10(machine.eps)))
     ;; cast that into the original type of our input number
     fdigits = fix(temporary(fdigits), type=type)
     mult = 10^(fdigits - round(alog10(abs(floor(number, L64=(max(abs(number)) gt 2d^30))) > 1)))
  endif

  ;; If we don't have a multiplier, we don't have any fancy work to do
  if NOT keyword_set(mult) then $
     return, retval

  ;; Make sure mult is in the correct type
  mult = fix(mult, type=type)

  ;; If we made it here, we are planning to do our mult/round dance.

  ;; Multiply by mult
  retval *= mult

  ;; Find idx of retval that are small enough to use round
  good_idx = where(abs(retval) lt 2d^63, count)
  ;; If none will fit in a 64-bit integer, return at this point
  if count eq 0 then begin
     ;; Put our retval back to pre-mult state (in case of /round)
     retval /= mult
     ;; Make sure we multiply by mult_in, if specified
     if N_elements(mult_in) ne 0 then $
        retval *= mult
     return, retval
  endif

  ;; Round
  if keyword_set(round) then begin
     ;; Set our longint flag for round
     L64 = 0
     if max(abs(retval)) gt 2d^30 then $
        L64=1

     ;; Now chop off our insignificant digits using round
     retval[good_idx] = round(retval[good_idx], l64=L64)
  endif ;; rounding

 ;; Set the type of our retval
 if keyword_set(L64) then $
    retval = long64(retval) $
 else $
    retval = long(retval)
 
 ;; Get ready to divide by mult, except in the case where we specified
 ;; mult_in.  In that case, retval is going to be an appropriately
 ;; typed integer
 if N_elements(mult_in) ne 0 then $
    mult = 1

  ;; Return our result
  return, retval / mult

end

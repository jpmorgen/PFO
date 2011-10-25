;+
; NAME: pfo_idx_parse
;
; PURPOSE: Parse indices returned from a pfo function __index "method"
; called with the /terminate_idx switch
;
; CATEGORY: PFO
;
; CALLING SEQUENCE: idx = pfo_idx_parse(out_idx, i)

; DESCRIPTION:  

;   The PFO system, particularly the __indices methods of pfo-enabled
;   functions can insert !tok.nowhere between segments of indices to
;   help separate them.  This is done when the /terminate_idx keyword
;   is set.  This function returns the ith set of indices from the
;   input array of terminated indices.

; INPUTS:

;   terminated_idx: an array of indices (presumably into a parinfo)
;   with !tok.nowhere used as termination marks for the individual
;   sets of indices.  If !tok.nowhere is not found, an error is
;   raised.  If no array is specified, quietly returns

;   i: the set of indices to return (starting from 0)

;
; OPTIONAL INPUTS:
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:

;   Returns the ith set of indices.  If i is greater than the number
;   of sets of indices, !tok.nowhere is returned.

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
; $Id: pfo_idx_parse.pro,v 1.1 2011/10/25 21:31:48 jpmorgen Exp $
;
; $Log: pfo_idx_parse.pro,v $
; Revision 1.1  2011/10/25 21:31:48  jpmorgen
; Initial revision
;
;-
function pfo_idx_parse, idx, ith

  init = {tok_sysvar}
  init = {pfo_sysvar}

  if !pfo.debug le 0 then begin
     CATCH, err
     if err ne 0 then begin
        CATCH, /CANCEL
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'USAGE: idx = pfo_idx_parse(terminated_idx, i)'
     endif
  endif ;; not debugging

  if N_elements(idx) eq 0 then $
     return, !tok.nowhere

  if N_elements(ith) eq 0 then $
     message, 'ERROR: Required positional parameter i not specified.'

  term_idx = where(idx eq !tok.nowhere, count)
  if count eq 0 then $
     message, 'ERROR: No instances of !tok.nowhere found in input array.  Did you forget /terminate_idx when you queried for indices?'

  if ith ge count then $
     return, !tok.nowhere

  ;; If we made it here, we have a valid segment of indices to return
  first = 0
  if ith gt 0 then $
     first = term_idx[ith-1] + 1
  return, idx[first:term_idx[ith]-1]
end

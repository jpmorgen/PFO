;+
; NAME: pfo_delimiter
;
; PURPOSE:  Figure out what delimiter to print between the parameter and its
; limit values
;
; CATEGORY: PFO print/widget
;
; CALLING SEQUENCE: d = pfo_delimiter(parinfo, side, params, idx, _REF_EXTRA=extra)
;
; DESCRIPTION: This function does some logic on the contents of params
; and the limits fields in the parinfo array to determine if the
; delimiter should be just < or <* (the * indicates pegged).  Regular
; . (free) and | (fixed) are also determined.
;
; INPUTS: 

; parinfo: must be listed first so works properly with
; pfo_obj::parinfo_call_function

; side: !pfo.left or !pfo.right.  Allows both delimiters to be handled
; by one routine

; params: parameter value (should be copied from parinfo.value or
; other source by the calling routine)

; OPTIONAL INPUTS:
;
; KEYWORD PARAMETERS:

; idx (required): index into parinfo of parameter to query

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
; $Id: pfo_delimiter.pro,v 1.3 2011/09/01 22:12:03 jpmorgen Exp $
;
; $Log: pfo_delimiter.pro,v $
; Revision 1.3  2011/09/01 22:12:03  jpmorgen
; *** empty log message ***
;
;-

function pfo_delimiter, parinfo, side, params, idx, _EXTRA=extra

  ;; Generic pfo system initialization
  init = {pfo_sysvar}

  ;; With all the nested function calls, sometimes errors get lost.
  ;; We won't get halt lines this way, but we will get the routine
  ;; name.

  ;; Handle pfo_debug level.  CATCH errors if _not_ debugging
  if !pfo.debug le 0 then begin
     CATCH, err
     if err ne 0 then begin
        CATCH, /CANCEL
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'ERROR: caught the above error.  Returning "?"', /CONTINUE
        return, '?'
     endif
  endif ;; not debugging

  if N_elements(idx) ne 1 then $
     message, 'ERROR: idx must has 1 and only 1 element.  It has ' + strtrim(N_elements(idx), 2)

  ;; Accomodate pfo_parinfo_delimiter_cw, which passes just one param
  if N_elements(params) eq N_elements(parinfo) then $
     value = params[idx]
  if N_elements(params) eq 1 then $
     value = params
  if N_elements(params) eq 0 then $
     value = parinfo[idx].value

  ;; Try to convey the most information possible with the delimiter.
  ;; Start with the normal free parameter
  delimiter = ' .  '
  ;; The case where we are limited on this side
  if parinfo[idx].limited[side] eq 1 then begin
     delimiter = ' <  '
     ;; check to see if we have hit the limit.  Use
     ;; mpmaxstep as a measure of "close"
     if abs(value - parinfo[idx].limits[side]) le $
       parinfo[idx].mpmaxstep then begin
        delimiter = ' <* '
     endif ;; close
  endif

  ;; Fixed
  if parinfo[idx].fixed eq 1 then $
    delimiter = ' |  '

  return, delimiter

end



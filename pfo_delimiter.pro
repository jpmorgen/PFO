; +
; $Id: pfo_delimiter.pro,v 1.1 2003/12/19 00:04:39 jpmorgen Exp $

; pfo_delimiter.pro 

;; Figure out what delimiter to print between the parameter and its
;; limit values

; -

function pfo_delimiter, side, params, parinfo, idx, _EXTRA=extra

  ;; Generic pfo system initialization
  init = {pfo_sysvar}

  ;; With all the nested function calls, sometimes errors get lost.
  ;; We won't get halt lines this way, but we will get the routine
  ;; name.

  CATCH, err
  if err ne 0 then begin
     CATCH, /CANCEL
     message, !error_state.msg
  endif

  ;; Try to convey the most information possible with the delimiter.
  ;; Start with the normal free parameter
  delimiter = ' .  '
  ;; The case where we are limited on this side
  if parinfo[idx].limited[side] eq 1 then begin
     delimiter = ' <  '
     ;; check to see if we have hit the limit.  Use
     ;; mpmaxstep as a measure of "close"
     if params[idx] - parinfo[idx].limits[side] lt $
       parinfo[idx].mpmaxstep then begin
        delimiter = ' <* '
     endif ;; close
  endif

  ;; Fixed
  if parinfo[idx].fixed eq 1 then $
    delimiter = '| '

  return, delimiter

end



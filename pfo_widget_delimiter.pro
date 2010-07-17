;+
; NAME: pfo_widget_delimiter
;
; PURPOSE: helper function that returns the index into
; !pfo_widget.delimiters and the boolean "pegged" based on information
; in parinfo
;
; This has to be its own little file because it can get referenced in
; pfo_funct when pfo_widget.prok is not compiled.
;
; CATEGORY: pfo_widget
;
; CALLING SEQUENCE:
;
; DESCRIPTION:
;
; INPUTS: side, params, parinfo, idx
;
; OPTIONAL INPUTS:
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;
; OPTIONAL OUTPUTS: pegged: keyword to indicate if value in params is
; too cluse
;
; COMMON BLOCKS:  
;   Common blocks are ugly.  Consider using package-specific system
;   variables.
;
; SIDE EFFECTS:
;
; RESTRICTIONS: Assumes that params has sensible values, as
; initialized by pfo_funct_check
;
; PROCEDURE:
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;
; $Id: pfo_widget_delimiter.pro,v 1.1 2010/07/17 18:57:24 jpmorgen Exp $
;-
;; Helper function for shifting the list around in the delimiter
;; droplist (e.g. free, limited, fixed).  Also returns pegged=1 if
;; param has hit a limit
function pfo_widget_delimiter, side, params, parinfo, idx, pegged=pegged, $
  sensitive=sensitive

  ;; Initialize our output variables
  pegged = ' '
  sensitive = 1
  ;; Handle both cases of parinfo on the stack and a "raw" parinfo
  pointer = 0
  if size(parinfo, /type) eq !tok.pointer then $
    pointer = 1

  ;; Get our basic information
  if pointer then begin
     fixed =  (*parinfo)[idx].fixed
     limit = (*parinfo)[idx].limits[side]
     limited = (*parinfo)[idx].limited[side]
     mpmaxstep = (*parinfo)[idx].mpmaxstep
     tied =  (*parinfo)[idx].tied
  endif else begin
     fixed =  parinfo[idx].fixed
     limit = parinfo[idx].limits[side]
     limited = parinfo[idx].limited[side]
     mpmaxstep = parinfo[idx].mpmaxstep
     tied =  parinfo[idx].tied
  endelse

  ;; Now do our logic to set the return value
  delimiter = !pfo_widget.free
  if limited eq 1 then begin
     delimiter = !pfo_widget.limited
     ;; check to see if we have hit the limit.  Use
     ;; mpmaxstep as a measure of "close"
     if abs(params[idx] - limit) le mpmaxstep then $
       pegged = '*'
  endif

  ;; Fixed
  if fixed eq 1 then begin
     ;; At the moment, it is not an error to have both fixed and
     ;; limited set.  Fixed has precedence.  But we don't want
     ;; to have the pegged flag hanging around.
     Delimiter = !pfo_widget.fixed
     pegged = ' '
  endif

  ;; Tied
  if keyword_set(tied) then begin
     ;; Make sure we don't imply that we can change the tied
     ;; information with this dropdown menu.  We need to go through
     ;; the pfo_link system or set .tied by hand
     sensitive = 0
     ;; Drive home that this parameter is tied.
     pegged = 'T'
  endif

  return, delimiter
  
end


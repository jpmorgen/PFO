;+
; NAME: pfo_min
;
; PURPOSE: find the minimum (maximum) of a PFO function
;
; CATEGORY: PFO
;
; CALLING SEQUENCE: Xin = pfo_min(parinfo, [startval], [idx=idx],
; [_REF_EXTRA=extra], [/MAXIMIZE])
;
; DESCRIPTION: uses Craig Markwardt's TNMIN routine to find the 
; minimum (maximum) value of the function described by parinfo[idx]
;
; INPUTS: parinfo -- parinfo array which defines the function
;
; OPTIONAL INPUTS: startval.  The Xin starting value
;
; KEYWORD PARAMETERS: 

; idx: the indices into parinfo for a sub-function (e.g. a peak). 

; _REF_EXTRA: keywords passed to and from TNMIN via the _REF_EXTRA
; mechanism.  See IDL documentation.  Also offered to the function.

; /MAXIMIZE: find the maximum value (actually passed via _REF_EXTRA to
; TNMIN)
	
;
; OUTPUTS: Xin value of the min (max) function value
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
; $Id: pfo_min.pro,v 1.1 2011/01/20 23:00:57 jpmorgen Exp $
;
; $Log: pfo_min.pro,v $
; Revision 1.1  2011/01/20 23:00:57  jpmorgen
; Initial revision
;
;-
function pfo_min_funct, Xin, parinfo=parinfo, idx=idx, _EXTRA=extra
  return, pfo_funct(Xin, parinfo=parinfo, idx=idx, _EXTRA=extra)
end

function pfo_min, parinfo, startval, idx=idx, _REF_EXTRA=extra

  init = {tok_sysvar}
  init = {pfo_sysvar}

  ;; Check parinfo
  nparinfo = N_elements(parinfo)
  if nparinfo eq 0 then $
    message, 'ERROR: no parinfo specified'
  if size(/type, parinfo) ne !tok.struct then $
    message, 'ERROR: invalid parinfo'

  ;; Check startval
  if N_elements(startval) eq 0 then $
    startval = 0
  if N_elements(startval) gt 1 then $
    message, 'ERROR: specify one and only one starting Xin value'

  ;; Check idx
  nidx = N_elements(idx)
  if nidx eq 0 then $
    idx = indgen(nparinfo, type=size(/type, nparinfo))
  nidx = N_elements(idx)
  if nidx gt nparinfo then $
    message, 'ERROR: too many idx values for this parinfo'


  ;; Get all of our links on board.
  pfo_link_check, parinfo

  ;; Prepare arguments for tnmin
  functargs = {parinfo:parinfo, idx:idx}
  if keyword_set(extra) then $
    pfo_struct_append, functargs, {_EXTRA:extra}
  
  ;; This is a very simple problem in one variable: Xin.  We just pass
  ;; parinfo via functargs to pfo_funct so the function gets
  ;; calculated properly.  TNMIN passes whatever Xin it is working with
  ;; (which it calls p).  We _don't_ pass parinfo to tnmin,
  ;; since it thinks we only have 1 parameter, Xin
  return, tnmin('pfo_min_funct', startval, functargs=functargs, $
                /autoderivative, _EXTRA=extra)

end


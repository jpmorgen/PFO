;+
; NAME: pfo_Xaxis
;
; PURPOSE: returns the Xaxis of a PFO functio
;
; CATEGORY: PFO
;
; CALLING SEQUENCE: Xaxis = pfo_Xaxis(parinfo, Xin=Xin, idx=idx, _REF_EXTRA=extra)

; DESCRIPTION: Uses where on parinfo.pfo.inaxis and .outaxis to search
; for the idx of parinfo functions tha make Xin to Xaxis
; transformation(s).  Just passes those idx to pfo_parinfo_parse for
; fastest calculation.

; INPUTS: parinfo: array of type structure that defines the PFO function
;
; OPTIONAL INPUTS:
;
; KEYWORD PARAMETERS:

;   Xin: X-axis in natural units

;   idx: optional indices into parinfo to consider subset of subfunctions

;   EXTRA arguments are passed directly to pfo_parinfo_parse and any
;   underlying routines (e.g. __calc "methods" of PFO functions)

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
; $Id: pfo_xaxis.pro,v 1.3 2011/09/01 22:25:55 jpmorgen Exp $
;
; $Log: pfo_xaxis.pro,v $
; Revision 1.3  2011/09/01 22:25:55  jpmorgen
; Significant improvements to parinfo editing widget, created plotwin
; widget, added pfo_poly function.
;
; Revision 1.2  2011/08/02 18:20:22  jpmorgen
; Release to Tom
; Improve handling of no parinfo
;
; Revision 1.1  2011/08/01 19:18:16  jpmorgen
; Initial revision
;
;-
function pfo_Xaxis, parinfo, Xin=Xin, idx=idx, _REF_EXTRA=extra

  init = {pfo_sysvar}
  init = {tok_sysvar}

  ;; Intrinsically makes sure Xin exists, sets default Xaxis if no
  ;; parinfo transformation of Xin to Xaxis exists
  Xaxis = Xin

  ;; If we don't have a parinfo, our work is done
  if N_elements(parinfo) eq 0 then $
     return, Xaxis

  ;; Make sure idx exists
  pfo_idx, parinfo, idx

  ;; Indices of any Xin to Xaxis transformations in idx
  Xaxis_idx = where(parinfo[idx].pfo.inaxis eq !pfo.Xin and $
                    parinfo[idx].pfo.outaxis eq !pfo.Xaxis, count)
  if count eq 0 then $
    return, Xaxis

  ;; unwrap
  Xaxis_idx = use_idx[Xaxis_idx]

  ;; If we made it here, we have a parinfo that defines an Xaxis

  ;; Pass pfo_parinfo_parse the Xaxis_idx and the Xin we accepted on
  ;; the command line, but other than that, rely on _EXTRA to pass all
  ;; other arguments through (including ispec, iROI, /allspec,
  ;; /allROI, etc.
  junk = pfo_parinfo_parse(/calc, parinfo, idx=Xaxis_idx, $
                           Xin=Xin, Xaxis=Xaxis, _EXTRA=extra)
  return, Xaxis

end

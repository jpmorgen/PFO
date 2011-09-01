;+
; NAME: pfo_ROI_Xin_idx
;
; PURPOSE: return indices into Xin calculated by the pfo_ROI functions
; in parinfo.  If there are no pfo_ROI functions, returns idx of
; entire Xin axis
;
; CATEGORY: PFO
;
; CALLING SEQUENCE:  Xin_idx = pfo_ROI_Xin_idx(parinfo, params=params,
; idx=idx, ispec=ispec, iROI=iROI, allspec=allspec, allROI=allROI,
; count=count, pfo_obj=pfo_obj, _EXTRA=extra)

; DESCRIPTION: This carefully performs the minimum set of calculations
; needed to return the indices into Xin of any requested set of
; pfo_ROI functions.  Special attention is given to also calculate the
; Xaxis if any of the pfo_ROI use Xaxis as a boundary instead of Xin.

; INPUTS: parinfo: parinfo array defining the functions
;
; OPTIONAL INPUTS:
;
; KEYWORD PARAMETERS:

;   count (output) number of indices returned

;   idx: indices into parinfo limiting which parinfo elements are
;   searched (default = all indices)

;   params: parameters to use in preference to 

;   ispec: (input) list of ispecs used in this parinfo.  If
;   defined on input, search for iROI is narrowed to the specified
;   ispecs.  Otherwise, all ispecs are used.

;   iROI: list of iROIs for which you want output indices.  Beware
;   that no attempt to separate iROIs from different ispecs is made in
;   this routine, so if you use the same iROIs, 0,1,2 in all of your ispecs, you
;   will get idx for ispecs with iROIs = [0,1,2,0,1,2, ...] unless you
;   narrow to just one ispec.

;
; OUTPUTS:

; Indices

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
; $Id: pfo_roi_xin_idx.pro,v 1.3 2011/09/01 22:26:26 jpmorgen Exp $
;
; $Log: pfo_roi_xin_idx.pro,v $
; Revision 1.3  2011/09/01 22:26:26  jpmorgen
; Significant improvements to parinfo editing widget, created plotwin
; widget, added pfo_poly function.
;
; Revision 1.2  2011/08/02 18:20:07  jpmorgen
; Release to Tom
; Improve handling of no parinfo
;
; Revision 1.1  2011/08/01 19:18:16  jpmorgen
; Initial revision
;
;-
function pfo_ROI_Xin_idx, $
   parinfo, $
   Xin=Xin, $
   idx=idx, $
   ispec=ispec, $
   iROI=iROI, $
   count=count, $
   pfo_obj=pfo_obj, $
   $ ;; most arguments of this function, including ispec and iROI are just passed along
   $ ;; to pfo_parinfo_parse with the _REF_EXTRA mechanism
   allspec=allspec, $ ;; intercept /allspec and /allROI, since we require them
   allROI=allROI, $ ;; and don't want to have duplicate keywords to pfo_parinfo_parse
   _REF_EXTRA=extra 

  init = {tok_sysvar}

  ;; Default output is entire idx of Xin
  pfo_idx, Xin, Xin_idx

  ;; If we have no parinfo, our job is done
  if N_elements(parinfo) eq 0 then $
     return, Xin_idx

  ;; Make sure idx into parinfo exists
  pfo_idx, parinfo, idx

  ;; Look for pfo_ROI functions
  ROI_idx = pfo_fidx(parinfo, 'pfo_ROI', idx=idx, nfunct=nfunct, pfo_obj=pfo_obj)
  ;; If there are none, our job is done, just return the entire idx
  ;; into Xin
  if nfunct eq 0 then $
     return, Xin_idx

  ;; If we made it here, we have some pfo_ROI functions to work with

  ;; Limit our ROI function(s) to the ones that the user wants
  ROI_idx = pfo_ROI_idx(parinfo, idx=ROI_idx, ispec=ispec, iROI=iROI, count=count)
  ;; Return with !tok.nowhere (and count=0) if no pfo_ROI functions
  ;; match the requested ispec(s)/iROI(s) do not
  if count eq 0 then $
     return, !tok.nowhere

  ;; Check to see if the pfo_ROI functions have any parameters that
  ;; require an X-axis (code from pfo_parinfo_parse)
  junk = where(parinfo[ROI_idx].pfo.inaxis eq !pfo.Xaxis, nXaxis)
  if nXaxis gt 0 then begin
     ;; Find the Xin to Xaxis map in parinfo[idx] (if any).  The
     ;; default is Xaxis = Xin, so it is not a problem if none are
     ;; found.
     Xaxis_idx = where(parinfo[idx].pfo.inaxis eq !pfo.Xin and $
                       parinfo[idx].pfo.outaxis eq !pfo.Xaxis, count)
     ;; Append the Xin to Xaxis mapping to our ROI_idx so that it
     ;; can be included in the calculation
     if count gt 0 then $
        pfo_array_append, ROI_idx, Xaxis_idx
  endif
  
  ;; Call pfo_parinfo_parse with the /allspec and /allROI keywords.
  ;; This is necessary since many Xin to Xaxis maps are flagged with
  ;; these ispec and iROI values.  This is OK, since above, we limited
  ;; our pfo_ROI functions in ROI_idx to just the ispec and iROI we
  ;; wanted.  We puffed them out with Xaxis_idx.  With /allspec and
  ;; /allROI, we make sure that the Xaxis_idx don't get
  ;; inappropriately taken back out again.
  junk = pfo_parinfo_parse(/calc, parinfo, Xin=Xin, idx=ROI_idx, $
                           /allspec, /allROI, ROI_Xin_idx=ROI_Xin_idx, $
                           pfo_obj=pfo_obj, _EXTRA=extra)

  count = N_elements(ROI_Xin_idx)
  if count eq 0 then $
     return, !tok.nowhere

  return, ROI_Xin_idx
end

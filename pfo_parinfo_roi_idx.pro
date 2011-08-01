;+
; NAME: pfo_ROI_idx
;
; PURPOSE: return indices into a parinfo of functions having 
; specific ispec(s) and iROI(s)
;
; CATEGORY: PFO
;
; CALLING SEQUENCE: ispec_iROI_idx = pfo_parinfo_ROI_idx(parinfo,
; count=count, idx=idx, ispec=ispec, iROI=iROI_in, pfo_obj=pfo_obj 
;
; DESCRIPTION: returns the indices into parinfo that have ispec=ispec
; and iROI=iROI.  ispec and iROI may be vectors.  iROIs not appearing
; in a particular ispec are just skipped.  If no ispec is specified,
; all ispec are used.  The same goes for iROI.
;
; INPUTS: parinfo: parinfo array describing function
;
; OPTIONAL INPUTS:
;
; KEYWORD PARAMETERS:

;   idx: indices into parinfo selecting a subset of function
;   (e.g. idx = pfo_fidx(parinfo, 'myfunct')

;   ispec: ispec value(s) to select.  

;   iROI: iROI value(s) to select.  Each ispec is searched for the
;   listed iROIs.  If a particular iROI is not found, it is simply skipped.
;
; OUTPUTS:

;   The list of indices into parinfo with elements having the specified
;   parinfo.pfo_ROI.ispec and parinfo.pfo_ROI.iROI values


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
; $Id: pfo_parinfo_roi_idx.pro,v 1.1 2011/08/01 19:18:16 jpmorgen Exp $
;
; $Log: pfo_parinfo_roi_idx.pro,v $
; Revision 1.1  2011/08/01 19:18:16  jpmorgen
; Initial revision
;
;-
function pfo_parinfo_ROI_idx, parinfo, count=count, idx=idx, ispec=ispec, $
                              iROI=iROI_in, pfo_obj=pfo_obj

  init = {tok_sysvar}

  ;; Set up default returns
  count = 0
  ROI_idx = -1 

  ;; Check to see if we have a pfo_ROI structure in a valid parinfo
  if NOT pfo_struct_tag_present(parinfo, 'pfo_ROI') then begin
     return, ROI_idx
  endif

  ;; Make sure idx exists
  pfo_idx, parinfo, idx=idx

  ;; Create our default array of ispec, which is all ispec in our parinfo[idx]
  if N_elements(ispec) eq 0 then begin
     junk = parinfo[idx].pfo_ROI.ispec
     u_idx = uniq(junk, sort(junk))
     ispec = junk[temporary(u_idx)]
  endif ;; default ispec

  ;; Cycle through our ispec and find the ROIs
  for is=0, N_elements(ispec)-1 do begin
     ispec_idx = where(parinfo[idx].pfo_ROI.ispec eq ispec[is])
     ;; unwrap
     ispec_idx = pfo_ROI_idx[ispec_idx]

     ;; If iROI is not specified, find all of them for this ispec
     if N_elements(iROI_in) eq 0 then begin
        junk = parinfo[ispec_idx].pfo_ROI.iROI
        u_idx = uniq(junk, sort(junk))
        iROI = junk[temporary(u_idx)]
     endif ;; finding all iROI for this ispec

     ;; Cycle through all the ROIs
     for iR=0, N_elements(iROI)-1 do begin
        iROI_idx = where(parinfo[ispec_idx].pfo_ROI.iROI eq iROI[iR], count)
        ;; Keep in mind that ROIs specified on the command line might
        ;; not be present in each ispec
        if count eq 0 then $
           CONTINUE
        ;; If we made it here, we are ready to collect our idx
        pfo_array_append, ROI_idx, temporary(iROI_idx)
     endfor ;; ROIs in this ispec
  endfor ;; ispec
 
  return, ROI_idx


end

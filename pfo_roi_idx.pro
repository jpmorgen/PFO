;+
; NAME: pfo_ROI_idx
;
; PURPOSE: return indices into a parinfo of functions having the
; specific ispec(s) and iROI(s)
;
; CATEGORY: PFO
;
; CALLING SEQUENCE:  use_idx = pfo_ROI_idx(parinfo, idx=idx,
; ispec=ispec, iROI=iROI, allspec=allspec, allROI=allROI, count=count)
;
; DESCRIPTION: Returns indices into parinfo of functions with selected
; ispec and iROI.  The entire list of iROI is compared to the iROIs in
; each ispec.  If you have the same set of iROIs in each ispec, this
; can be either convenient or annoying.  If you want to make sure you
; get to a particular iROI in a particular ispec, do not supply more
; than one ispec at a time.
;
; INPUTS: parinfo: parinfo array defining the functions
;
; OPTIONAL INPUTS:
;
; KEYWORD PARAMETERS:

;   idx: indices into parinfo limiting which parinfo elements are
;   searched (default = all indices)

;   ispec: (input) list of ispecs used in this parinfo.  If
;   defined on input, search for iROI is narrowed to the specified
;   ispecs.  Otherwise, all ispecs are used.

;   iROI: list of iROIs for which you want output parinfo indices.
;   Beware that no attempt to separate iROIs from different ispecs is
;   made in this routine, so if you use the same iROIs, 0,1,2 in all
;   of your ispecs, you will get idx for ispecs with iROIs =
;   [0,1,2,0,1,2, ...] unless you narrow to just one ispec.

;   count (output): number of indices into parinfo found that match
;   our (optional) ispec/iROI input criterion

;
; OUTPUTS: 

;   function returns indices into parinfo that match our (optional)
;   ispec/iROI input criterion.  If no ispec/iROI are specified,
;   entire idx of parinfo is returned.

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
; $Id: pfo_roi_idx.pro,v 1.2 2011/09/01 22:27:28 jpmorgen Exp $
;
; $Log: pfo_roi_idx.pro,v $
; Revision 1.2  2011/09/01 22:27:28  jpmorgen
; Significant improvements to parinfo editing widget, created plotwin
; widget, added pfo_poly function.
;
; Revision 1.1  2011/08/01 19:18:16  jpmorgen
; Initial revision
;
;-
function pfo_ROI_idx, $
   parinfo, $
   idx=idx, $
   ispec=ispec_in, $
   iROI=iROI_in, $
   allspec=allspec, $
   allROI=allROI, $
   count=count_return

  init = {tok_sysvar}
  init = {pfo_sysvar}

  ;; Make sure idx exists
  pfo_idx, parinfo, idx

  ;; Our default is to return all indices unless the user specifies
  ;; ispec and/or iROI
  count_return = N_elements(idx)

  ;; Handle the case of no input ispec and iROI (return all indices)
  if N_elements(ispec_in) + N_elements(iROI_in) eq 0 then $
     return, idx

  ;; If we made it here, the user thinks that the parinfo has some
  ;; ispec/iROI information

  ;; Use pfo_ROI_list to convert from ispec,iROI input (if any) to
  ;; valid ispec_iROI
  ok = pfo_ROI_list(parinfo, idx=idx, ispec_in=ispec_in, iROI_in=iROI_in, $
                    ispec_out=ispec, iROI_out=iROI, N_ispec_out=N_ispec, N_iROI_out=N_iROI) 
  if NOT ok then $
     message, 'ERROR: parinfo structure does not have the pfo_ROI tag, so I cannot find any ispec or iROI in it.  Did you forget to use "pfo_parinfo_update, parinfo, required_tags=''pfo_ROI''?"'
  if N_iROI eq 0 then begin
     message, /INFORMATIONAL, 'NOTE: input ispec = ' + pfo_array2string(ispec_in) + ' iROI = ' + pfo_array2string(iROI_in) + ' combination not found in parinfo'
     count_return = 0
     return, !tok.nowhere
  endif ;; no matching iROIs found

  ;; If we made it here, we have a valid set of ispec and iROI

  ;; Handle the /allspec and /allROI keywords
  if keyword_set(allspec) then begin
     ;; Add !pfo.allspec to ispec list if it is not already there
     junk = where(ispec eq !pfo.allspec, count)
     ;; Normally I would use pfo_array_append, but I happen to know
     ;; that !pfo.allspec = -1, so doing it this way makes the sort
     ;; in pfo_parinfo_parse a little more efficient.
     if count eq 0 then $
        ispec = [!pfo.allspec, temporary(ispec)]
  endif
  if keyword_set(allROI) then begin
     ;; Add !pfo.allROI to iROI list if it is not already there
     junk = where(iROI eq !pfo.allROI, count)
     ;; Normally I would use pfo_array_append, but I happen to know
     ;; that !pfo.allspec = -1, so doing it this way makes the sort
     ;; in pfo_parinfo_parse a little more efficient.
     if count eq 0 then $
        iROI = [!pfo.allROI, temporary(iROI)]
  endif

  ;; Cycle through our ispecs and find matching iROIs
  for is=0, N_elements(ispec)-1 do begin
     ;; Cycle through the ROIs
     for iR=0, N_elements(iROI)-1 do begin
        tidx = where(parinfo[idx].pfo_ROI.ispec eq ispec[is] and $
                     parinfo[idx].pfo_ROI.iROI  eq  iROI[iR], count)
        if count eq 0 then $
           CONTINUE

        ;; If we made it here, we have a valid ispec iROI pair for a
        ;; set of idx
        ;; unwrap
        tidx = idx[tidx]
        pfo_array_append, idx_out, tidx

     endfor ;; iROIs
  endfor ;; ispecs

  ;; Check to see if we have found anything.
  count_return = N_elements(idx_out)
  if count_return eq 0 then $
     return, !tok.nowhere
  return, idx_out

end

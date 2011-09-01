;+
; NAME: pfo_ROI_list
;
; PURPOSE:  Returns lists all of the ispec and iROI in parinfo[idx].
; Used in conjunction with pfo_ROI_idx this can select functions
; acting on particular ispec and ROIs
;
; CATEGORY: PFO
;
; CALLING SEQUENCE: ok = pfo_ROI_list(parinfo, idx=idx,
; ispec_in=ispec_in, iROI_in=iROI_in, ispec_out=ispec_out,
; iROI_out=iROI_out, N_ispec_out=N_ispec_out, N_iROI_out=N_iROI_out


; DESCRIPTION: This routine is useful for probing the ispec and iROI
; contents of parinfo[idx].  It is a function that returns -1 if the
; pfo_ROI tag is not present, 0 if you specified ispec_in(s) that
; ended up not being found in the parinfo and 1 if at least some
; matching ispec was found.  If no ispec_in were specified, all ispecs
; in parinfo[idx] are returned on ispec_out.  All iROI _on each ispec_
; matching iROI_in are returned on iROI_out.  As with ispec, if no
; iROI_in is specified, all iROI in parinfo[idx] are returned.

; INPUTS: parinfo: parinfo array defining the functions
;
; OPTIONAL INPUTS:
;
; KEYWORD PARAMETERS:
;   idx: indices into parinfo limiting which parinfo elements are
;   searched (default = all indices)

;   ispec_in (optional): list of ispecs used to narrow search.  If not
;   specified, all ispecs in parinfo[idx] are returned on ispec_out.
;   If specified, only those ispecs in ispec_in that are found in
;   parinfo[idx] are returned in ispec_out

;   ispec_out (output): output list of ispecs (see ispec_in). If no
;   matches between ispec_in and the ispecs in parinfo[idx] were
;   found, ispec_out='' (null string)

;   iROI_in (optional): list of iROIs to match in each ispec.  The
;   iROIs in each ispec are searched for iROI_in.  Any matches are
;   returned on iROI_out.  If no iROI_in is specified, all iROIs from
;   all the ispec_out are returned in iROI_out

;   iROI_out (output): list of iROIs in parinfo[idx], optionally
;   narrowed to ispec_in and iROI_in.  If no iROI_in matches were
;   found, iROI_out='' (null string)

;   N_ispec_out: number of valid ispecs in ispec_out
;   N_iROI_out: number of valid ispecs in iROI_out

; OUTPUTS:

;  function returns 1 if a valid pfo_ROI tag was found in parinfo, 0 otherwise

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
; $Id: pfo_roi_list.pro,v 1.2 2011/09/01 22:27:20 jpmorgen Exp $
;
; $Log: pfo_roi_list.pro,v $
; Revision 1.2  2011/09/01 22:27:20  jpmorgen
; Significant improvements to parinfo editing widget, created plotwin
; widget, added pfo_poly function.
;
; Revision 1.1  2011/08/01 19:18:16  jpmorgen
; Initial revision
;
;-
function pfo_ROI_list, $
   parinfo, $
   idx=idx, $
   ispec_in=ispec_in, $
   iROI_in=iROI_in, $
   ispec_out=ispec_out, $
   iROI_out=iROI_out, $
   N_ispec_out=N_ispec_out, $
   N_iROI_out=N_iROI_out

  init = {tok_sysvar}
  init = {pfo_sysvar}

  ;; Initialize our outputs
  ispec_out = ''
  iROI_out = ''
  N_ispec_out = 0
  N_iROI_out = 0

  ;; Handle pfo_debug level.  CATCH errors if _not_ debugging
  if !pfo.debug le 0 then begin
     CATCH, err
     if err ne 0 then begin
        CATCH, /CANCEL
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'ERROR: caught the above error.  Returning 0', /CONTINUE
        return, 0
     endif
  endif ;; not debugging

  ;; Check to see if we have a pfo_ROI structure in a valid parinfo.
  ;; If not, quietly return 0
  if NOT pfo_struct_tag_present(parinfo, 'pfo_ROI') then $
     return, 0

  ;; Make sure idx exists
  pfo_idx, parinfo, idx

  ;; Work from local variables
  if N_elements(ispec_in) ne 0 then $
     ispec = ispec_in
  if N_elements(iROI_in) ne 0 then $
     iROI = iROI_in

  ;; Handle the case of no input ispec by considering all ispec
  if N_elements(ispec) eq 0 then begin
     junk = parinfo[idx].pfo_ROI.ispec
     u_idx = uniq(junk, sort(junk))
     ispec = junk[temporary(u_idx)]
  endif ;; get our ispec list, if not specified
  ;; Ditto for iROI
  if N_elements(iROI) eq 0 then begin
     junk = parinfo[idx].pfo_ROI.iROI
     u_idx = uniq(junk, sort(junk))
     iROI = junk[temporary(u_idx)]
  endif ;; get our iROI list, if not specified

  ;; Cycle through our ispecs, building up out output ispec and iROI
  for is=0, N_elements(ispec)-1 do begin
     ispec_idx = where(parinfo[idx].pfo_ROI.ispec eq ispec[is], count)
     ;; In the case that the user specified ispec_in, check to make
     ;; sure there is a match
     if count eq 0 then $
        CONTINUE
     ;; unwrap
     ispec_idx = idx[ispec_idx]

     ;; If we made it here, this ispec exists.  Add it to our
     ;; validated ispec output list
     pfo_array_append, ispec_out, ispec[is]

     ;; Find all ROIs in this ispec
     junk = parinfo[ispec_idx].pfo_ROI.iROI
     u_idx = uniq(junk, sort(junk))
     t_iROI = junk[temporary(u_idx)]
     ;; Now find matches with our input iROI
     for iR=0, N_elements(iROI)-1 do begin
        junk = where(iROI[iR] eq t_iROI, count)
        if count eq 0 then $
           CONTINUE
        ;; If we made it here, we have a valid iROI match
        pfo_array_append, iROI_out, iROI[iR]
     endfor ;; each iROI
  endfor ;; each ispec

  ;; Construct our number outputs.  If we have strings, that
  ;; means we didn't accumulate anything
  N_ispec_out = N_elements(ispec_out)
  if size(/type, ispec_out) eq !tok.string then $
     N_ispec_out = 0
  ;; Construct our number outputs
  N_iROI_out = N_elements(iROI_out)
  if size(/type, iROI_out) eq !tok.string then $
     N_iROI_out = 0

  return, 1

end

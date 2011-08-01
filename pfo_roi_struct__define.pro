;+
; NAME: pfo_roi_struct__define
;
; PURPOSE: Define, initialize and work with the structure PFO_ROI_STRUCT.
;
; CATEGORY: PFO
;
; CALLING SEQUENCE: 

; DESCRIPTION: 

; The PFO_ROI tag of the parinfo enables the input X-axis of the
; parinfo system to be divided into multiple independent regions of
; interest (ROI).  Since PFO has its roots in spectroscopy, a second
; specifier, ispec, is added so that multiple spectra can be
; simultaneously processed.  parinfo.pfo_ROI.ispec can be used to
; associate particular functions to a particular portion of the data.
; For instance, by arranging the Xin, Y, and Yerr inputs as Nspec x
; Mchannel arrays, ispec can be used to associate functions with the
; ith Mchannel spectrum.  

; ROIs work with the ROI function (pfo_roi__fdefine).  See that file
; for more details.

; INPUTS:
;
; OPTIONAL INPUTS:
;
; KEYWORD PARAMETERS:
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
; $Id: pfo_roi_struct__define.pro,v 1.1 2011/08/01 19:18:16 jpmorgen Exp $
;
; $Log: pfo_roi_struct__define.pro,v $
; Revision 1.1  2011/08/01 19:18:16  jpmorgen
; Initial revision
;
;-

;; The __print "method" is used in full printing to print ispec and
;; iROI _on the same line as the equation_.  
pro pfo_ROI_struct__print, $
   parinfo, $
   idx=idx, $
   equation_string=equation_string
;;   col_head=col_head, $

;;  ;; Print header
;;  if keyword_set(col_head) then begin
;;     col_head += ' ispec iROI '
;;     return
;;  endif

  ;; Print ispec and iROI on equation line, since they apply to the
  ;; entire function
  if keyword_set(equation_string) then begin
     equation_string += string(format='(" ispec=", I5, " iROI=", I4)', $
                               parinfo[idx[0]].pfo_ROI.ispec, $
                               parinfo[idx[0]].pfo_ROI.iROI)
     return
  endif  

end

;; A __get_tag routine is optional but recommended.  It lets you map
;; tagnames to keywords in a procedure call
;; (e.g. pfo_struct_setget_tag).  This is the analog of a get_property
;; routine.  Although it is tedious to list all of the keywords/tags,
;; doing it this way passes values by reference and ultimately saves
;; memory.  Also, keywords don't necessarily have to map to
;; top-level keywords in <tag>_struct.  In other words, your struct
;; can be quite complicated and a properly set-up __set/get_tag
;; routines help you get the most out of it.

pro pfo_ROI_struct__get_tag, $
   parinfo, idx=idx, $
   taglist_series= taglist_series, $ ;; See pfo_setget_tag
   taglist_strict= taglist_strict, $ ;; See pfo_setget_tag
   _REF_EXTRA   = extra, $
   ispec    	= ispec, $
   iROI    	= iROI, $
   ROI_color	= ROI_color

   init = {pfo_sysvar}
  init = {tok_sysvar}
  if !pfo.debug le 0 then begin
     ;; Return to the calling routine with our error
     ON_ERROR, !tok.return
     CATCH, err
     if err ne 0 then begin
        CATCH, /CANCEL
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'USAGE: pfo_struct__get_tag, parinfo, idx=idx, [tag=tag, ...], [taglist_series=taglist_series, [/taglist_strict]]'
     endif
  endif ;; not debugging

  ;; Put our struct into a tag, if necessary
  pfo_struct_tagify, parinfo, 'PFO_ROI', tagified=tagified

  ;; If we made it here, we are good to copy our tags into the
  ;; keywords
  if N_elements(idx) eq 0 then $
     idx = lindgen(N_elements(parinfo))

  if arg_present(ispec) or N_elements(ispec) ne 0 then ispec = parinfo[idx].pfo_ROI.ispec
  if arg_present(iROI ) or N_elements(iROI ) ne 0 then iROI  = parinfo[idx].pfo_ROI.iROI
  if arg_present(ROI_color) or N_elements(ROI_color ) ne 0 then ROI_color  = parinfo[idx].pfo_ROI.ROI_color

  ;; Put our struct into a tag, if necessary
  pfo_struct_tagify, parinfo, 'PFO_ROI', tagified=tagified

  ;; Pass on keywords processed in series to the next top-level tag
  ;; listed in taglist_series.  
  pfo_struct_setget_tag, parinfo, idx=idx, /next, /get, $
                         taglist_series=taglist_series, $
                         taglist_strict=taglist_strict, $
                         _EXTRA=extra

end 

;; A __set_tag routine is optional, but recommended.  It saves some
;; code in the __init "method" and makes it easy to convert from
;; keywords to tag assignments with pfo_struct_setget_tag.  This is
;; the analogy of a set_property routine
pro pfo_ROI_struct__set_tag, $
   parinfo, idx=idx, $
   taglist_series= taglist_series, $ ;; See pfo_set_tag
   taglist_strict= taglist_strict, $ ;; See pfo_set_tag
   _REF_EXTRA   	= extra, $
   ispec    	= ispec, $
   iROI    	= iROI, $
   ROI_color	= ROI_color                          

  init = {pfo_sysvar}
  init = {tok_sysvar}
  if !pfo.debug le 0 then begin
     ;; Return to the calling routine with our error
     ON_ERROR, !tok.return
     CATCH, err
     if err ne 0 then begin
        CATCH, /CANCEL
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'USAGE: pfo_ROI_struct__set_tag, parinfo, idx=idx, [tag=tag, ...], [taglist_series=taglist_series, [/taglist_strict]]'
     endif
  endif ;; not debugging

  ;; Put our struct into a tag, if necessary
  pfo_struct_tagify, parinfo, 'PFO_ROI', tagified=tagified

  ;; If we made it here, we are good to copy our keywords into the
  ;; tags
  if N_elements(idx) eq 0 then $
    idx = lindgen(N_elements(parinfo))

  if N_elements(ispec) ne 0 then parinfo[idx].pfo_ROI.ispec = ispec
  if N_elements(iROI ) ne 0 then parinfo[idx].pfo_ROI.iROI  = iROI
  if N_elements(ROI_color ) ne 0 then parinfo[idx].pfo_ROI.ROI_color  = ROI_color

  ;; Put our tag back on the top-level, if necessary
  pfo_struct_tagify, parinfo, 'PFO_ROI', tagified=tagified

  ;; Pass on keywords processed in series to the next top-level tag
  ;; listed in taglist_series.  
  pfo_struct_setget_tag, parinfo, idx=idx, /next, /set, $
                      taglist_series=taglist_series, $
                      taglist_strict=taglist_strict, $
                      _EXTRA=extra

end 

function pfo_ROI_struct__init, $
  descr=descr, $ ;; descr is a return keyword that contains the structure documentation
  _REF_EXTRA=extra ;; keyword parameters to pass by reference (to save memory) to our __set_tag routine

  ;; Read in our pfo tokens
  init = {tok_sysvar}
  init = {pfo_sysvar}

  ;; Create our struct initialized with generic IDL null values.  We
  ;; could also have called:
  ;; pfo_struct = create_struct(name='pfo_ROI_struct')
  pfo_ROI_struct = {pfo_ROI_struct}

  ;; Initialize our ispec and iROI values so that new functions are
  ;; calculated over all spectra and all ROIs by default.
  pfo_ROI_struct.ispec = !pfo.allspec
  pfo_ROI_struct.iROI = !pfo.allROI
  pfo_ROI_struct.ROI_color = !tok.nowhere

  ;; Create our description
  descr = $
    {README	: 'Contains tags related to mapping functions to data axes', $
     ispec	: 'Integer indicating which "spectrum" a particular parameter is mapped to in a Mspec x Nchannel data environment (where Nchannel is optionally a multi-dimensional array)', $
     iROI	: 'Integer indicating which ROI a parameter is associated with.  The ROI function, of necessity uses iROI', $
     ROI_color	: 'Byte to be mapped to a color in the rainbow18 color table (default PFO color table) or other direct-graphics color table scheme'}


  ;; The last thing we do is pass on any other keywords to our
  ;; __set_tag "method."  Do this with _STRICT_EXTRA to make sure that
  ;; no bogus keywords are passed.  Be careful with debugging and make
  ;; sure user gets the best effort case when we are trying to ignore
  ;; the error.
  if !pfo.debug le 0 then begin
     CATCH, err
     if err ne 0 then begin
        message, /NONAME, !error_state.msg, /CONTINUE
        message, /INFORMATIONAL, 'WARNING: the above error was produced.  Use pfo_debug to help fix error, pfo_quiet, to suppress reporting (not recommended).' 
        return, parinfo 
     endif ;; CATCH
  endif ;; debugging
  pfo_ROI_struct__set_tag, pfo_ROI_struct, _STRICT_EXTRA=extra

  return, pfo_ROI_struct

end

;; Standard IDL named structure definition 
pro pfo_ROI_struct__define
  pfo_ROI_struct = $
    {pfo_ROI_struct, $
     $ ;; Make iROI and ispec type integer so that we have plenty of room to grow.  Long would be overkill
     ispec	: 0, $
     iROI	: 0, $
     $ ;; ROI_color needs to be int so it can be set to -1 to signal can be byte, since it at most is going to be used in a direct graphics color table (more likely just to select color in rainbow18)
     ROI_color	: 0}
end

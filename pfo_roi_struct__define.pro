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
; $Id: pfo_roi_struct__define.pro,v 1.8 2011/12/01 22:08:24 jpmorgen Exp $
;
; $Log: pfo_roi_struct__define.pro,v $
; Revision 1.8  2011/12/01 22:08:24  jpmorgen
; Minor change in documentation
;
; Revision 1.7  2011/11/18 16:07:44  jpmorgen
; Comment updates
;
; Revision 1.6  2011/09/22 23:46:30  jpmorgen
; Get rid of FSC_field and no more segmentation faults on LINUX!
;
; Revision 1.5  2011/09/16 13:48:57  jpmorgen
; Simplified widget hierarchy to try to speed up
;
; Revision 1.4  2011/09/08 20:08:20  jpmorgen
; Cleaned up/created update of widgets at pfo_parinfo_obj level
;
; Revision 1.3  2011/09/01 22:15:21  jpmorgen
; Significant improvements to parinfo editing widget, created plotwin
; widget, added pfo_poly function.
;
; Revision 1.2  2011/08/17 02:41:42  jpmorgen
; About to delete some code I found handy to fix a problem I may have
; fixed a better way
;
; Revision 1.1  2011/08/01 19:18:16  jpmorgen
; Initial revision
;
;-

;; Helper procedure returns current ispec and iROI
pro pfo_ROI_struct_cw_obj::get_iROI_ispec, ispec=ispec, iROI=iROI

  ;; Get output ROI_struct and ROI_struct current values
  self.pfo_obj->parinfo_call_procedure, $
     /no_update, 'pfo_struct_setget_tag', /get, idx=*self.pidx, $
     taglist_series=['pfo', 'pfo_ROI'], ispec=ispec, iROI=iROI, ftype=ftype

  junk = uniq(iROI, sort(iROI))
  if N_elements(junk) ne 1 then begin
     message, 'ERROR: more than one iROI specified for ' + pfo_fname(ftype[0], pfo_obj=self.pfo_obj) + ' idx = ' + pfo_array2str(*self.pidx) + '.  If this function can really work that way, call this widget on each parameter individually'
  endif

  ;; If we made it here, the first element should be all we need
  ispec = ispec[0]
  iROI = iROI[0]

end

function pfo_ROI_struct_cw_obj::event, event

  ;; We will always swallow the event
  retval = !tok.nowhere

  ;; Check to see if we have changed our values
  self->get_iROI_ispec, ispec=ispec, iROI=iROI

  widget_control, self.ispecID, get_value=w_ispec
  widget_control, self.iROIID, get_value=w_iROI

  ;; Check to see if user has erased value in field.  In this case
  ;; fsc_field returns NAN
  if NOT finite(w_ispec) or NOT finite(w_iROI) then begin
     ;; Put existing values back.  This just effects our widget.
     self->refresh
     return, retval
  endif

  ;; No change means swallow the event
  if ispec eq w_ispec and iROI eq w_iROI then $
     return, retval

  ;; If we made it here, we have a valid change

  ;; Write it into the parinfo
  self.pfo_obj->parinfo_call_procedure, $
     /save_undo, 'pfo_struct_setget_tag', /set, idx=*self.pidx, $
     taglist_series='pfo_ROI', ispec=w_ispec, iROI=w_iROI

  return, retval
end

;; Refresh method.  pfo_cw_obj handles error catching
pro pfo_ROI_struct_cw_obj::refresh

  ;; Refresh our display
  self->get_iROI_ispec, ispec=ispec, iROI=iROI
  widget_control, self.ispecID, set_value=ispec
  widget_control, self.iROIID, set_value=iROI

end

pro pfo_ROI_struct_cw_obj::populate, $
   _REF_EXTRA=extra ;; for now, swallow any extra keywords

  self->get_iROI_ispec, ispec=ispec, iROI=iROI

  ;; Try pfo_cw_field for these widgets
  self.ispecID = pfo_cw_field(self.tlbID, title='ispec:', xsize=5, $
                              value=ispec, /integer, /noedit)
  self.iROIID = pfo_cw_field(self.tlbID, title='iROI:', xsize=5, $
                             value=iROI, /integer, /return_events, $
                             /kbrd_focus_events, $
                             uvalue={method:'event', obj:self})

  ;; Register ourselves in the refresh list.  
  self->register_refresh

end

;; Cleanup method
pro pfo_ROI_struct_cw_obj::cleanup
  ;; Turn off keyboard focus before we kill our widgets so extra
  ;; keyboard focus events during death don't cause segmentation
  ;; faults
  if obj_valid(self.ispec_obj) then begin
     self.ispec_obj->SetProperty, event_func=''
     self.iROI_obj->SetProperty, event_func=''
  endif ;; if fsc_field widgets still exist, turn off event processing until they die

  ;; Call our inherited cleaup routines
  self->pfo_parinfo_cw_obj::cleanup
end

;; Init method
function pfo_ROI_struct_cw_obj::init, $
   parentID, $ ;; widgetID of parent widget
   _REF_EXTRA=extra ;; All other input parameters are passed to underlying routines via _REF_EXTRA

  ;; Handle pfo_debug level.  CATCH errors if _not_ debugging
  if !pfo.debug le 0 then begin
     CATCH, err
     if err ne 0 then begin
        CATCH, /CANCEL
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'ERROR: caught the above error.  Object not properly initialized ', /CONTINUE
        return, 0
     endif
  endif ;; not debugging

  ;; Call our inherited init routines.  This puts pfo_obj into self,
  ;; among other things.  Make the tlb a row base, so ispec and iROI
  ;; appear side-by-side
  ok = self->pfo_parinfo_cw_obj::init(parentID, /row, _EXTRA=extra)
  if NOT ok then return, 0

  ;; Register with the refresh list
  self->register_refresh

  ;; Build our widget
  self->populate, _EXTRA=extra

  ;; If we made it here, we have successfully set up our widget.
  return, 1

end

;; Object class definition
pro pfo_ROI_struct_cw_obj__define
  objectClass = $
     {pfo_ROI_struct_cw_obj, $
      ispecID	: 0L, $ ;; widget ID(s) used in refresh method
      iROIID	: 0L, $ ;; widget ID(s) used in refresh method
      ispec_obj	: obj_new(), $ ;; object controlling fsc_field widget
      iROI_obj	: obj_new(), $ ;; object controlling fsc_field widget
      inherits pfo_parinfo_cw_obj}
end

;; The __widget "method" is used to display ispec and iROI on the same
;; line as the equation, since they, since they apply to the entire
;; function.  Because this is used in the
;; pfo_obj->parinfo_call_procedure and pfo_struct_call_procedure
;; systems, parinfo must be the first poositional argument.  Because
;; it is a widget, the parent widget ID should not be far behind!

function pfo_ROI_struct__widget, $
   parinfo, $ ;; entire parinfo array passed by reference (usually from pfo_obj)
   parentID, $ ;; The rowID of our equation line
   equation=equation, $ ;; flag to trigger our code
   _REF_EXTRA=extra

  ;; Initialize output
  cwID = !tok.nowhere

  ;; If we are not on the equation line, we have nothing to do
  if NOT keyword_set(equation) then $
     return, cwID

  ;; Create our controlling object
  cw_obj = obj_new('pfo_roi_struct_cw_obj', parentID, _EXTRA=extra)

  ;; The init method creates the widget and stores its ID in self.tlb.
  ;; Use the getID method to access it.  We return this ID, since that
  ;; is what people expect when they call a widget creation function.
  ;; What people will probably really want is the object to do the
  ;; heavy-duty control.  Default to a nonsense widgetID unless the
  ;; object creation was sucessful.
  if obj_valid(cw_obj) then begin
     cwID = cw_obj->tlbID()
  endif ;; valid cw_obj

end

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
   taglist_series= taglist_series, $ ;; See pfo_struct_setget_tag
   strict= strict, $ ;; See pfo_struct_setget_tag
   _REF_EXTRA   = extra, $
   ispec    	= ispec, $
   iROI    	= iROI, $
   ROI_color	= ROI_color

  if !pfo.debug le 0 then begin
     CATCH, err
     if err ne 0 then begin
        CATCH, /CANCEL
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'ERROR: caught the above error.  Returning with what I have done so far ', /CONTINUE
        return
     endif
  endif ;; not debugging

  ;; Make sure idx exists
  pfo_idx, parinfo, idx

  ;; Put our struct into a tag, if necessary
  pfo_struct_tagify, parinfo, tagified=tagified

  ;; If we made it here, we are good to copy our tags into the
  ;; keywords

  if arg_present(ispec) or N_elements(ispec) ne 0 then ispec = parinfo[idx].pfo_ROI.ispec
  if arg_present(iROI ) or N_elements(iROI ) ne 0 then iROI  = parinfo[idx].pfo_ROI.iROI
  if arg_present(ROI_color) or N_elements(ROI_color ) ne 0 then ROI_color  = parinfo[idx].pfo_ROI.ROI_color

  ;; Put our struct into a tag, if necessary
  pfo_struct_tagify, parinfo, tagified=tagified

  ;; Pass on keywords processed in series to the next top-level tag
  ;; listed in taglist_series.  
  pfo_struct_setget_tag, parinfo, idx=idx, /next, /get, $
                         taglist_series=taglist_series, $
                         strict=strict, $
                         _EXTRA=extra

end 

;; A __set_tag routine is optional, but recommended.  It saves some
;; code in the __init "method" and makes it easy to convert from
;; keywords to tag assignments with pfo_struct_setget_tag.  This is
;; the analogy of a set_property routine
pro pfo_ROI_struct__set_tag, $
   parinfo, idx=idx, $
   taglist_series= taglist_series, $ ;; See pfo_set_tag
   strict= strict, $ ;; See pfo_set_tag
   _REF_EXTRA   	= extra, $
   ispec    	= ispec, $
   iROI    	= iROI, $
   ROI_color	= ROI_color                          

  if !pfo.debug le 0 then begin
     CATCH, err
     if err ne 0 then begin
        CATCH, /CANCEL
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'ERROR: caught the above error.  Returning with what I have done so far ', /CONTINUE
        return
     endif
  endif ;; not debugging

  ;; Make sure idx exists
  pfo_idx, parinfo, idx

  ;; Put our struct into a tag, if necessary
  pfo_struct_tagify, parinfo, tagified=tagified

  ;; If we made it here, we are good to copy our keywords into the
  ;; tags

  if N_elements(ispec) ne 0 then parinfo[idx].pfo_ROI.ispec = ispec
  if N_elements(iROI ) ne 0 then parinfo[idx].pfo_ROI.iROI  = iROI
  if N_elements(ROI_color ) ne 0 then parinfo[idx].pfo_ROI.ROI_color  = ROI_color

  ;; Put our tag back on the top-level, if necessary
  pfo_struct_tagify, parinfo, tagified=tagified

  ;; Pass on keywords processed in series to the next top-level tag
  ;; listed in taglist_series.  
  pfo_struct_setget_tag, parinfo, idx=idx, /next, /set, $
                      taglist_series=taglist_series, $
                      strict=strict, $
                      _EXTRA=extra

end 

function pfo_ROI_struct__init, $
  descr=descr, $ ;; descr is a return keyword that contains the structure documentation
  _REF_EXTRA=extra ;; keyword parameters to pass by reference (to save memory) to our __set_tag routine

  ;; Create our struct initialized with generic IDL null values.  We
  ;; could also have called:
  ;; pfo_struct = create_struct(name='pfo_ROI_struct')
  pfo_ROI_struct = {pfo_ROI_struct}
  
  ;; Initialize our ispec and iROI values to so that without other
  ;; customization, new functions are all mathematically linked across
  ;; all spectra and all ROIs.  In other words, you can define just
  ;; one continuum which will apply to all spectra in the dataset and
  ;; (more commonly) across all ROIs in each spectrum.  The
  ;; alternative is using mathematically independent ROIs (iROI ge 0)
  ;; and multiple, identical functions with parameters linked to each other.
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
        CATCH, /CANCEL
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

  ;; Read in system variables for all routines in this file.
  init = {pfo_sysvar}
  init = {tok_sysvar}

  ;; Define our named structure
  pfo_ROI_struct = $
    {pfo_ROI_struct, $
     $ ;; Make iROI and ispec type integer so that we have plenty of room to grow.  Long would be overkill
     ispec	: 0, $
     iROI	: 0, $
     $ ;; ROI_color needs to be int so it can be set to -1 to signal can be byte, since it at most is going to be used in a direct graphics color table (more likely just to select color in rainbow18)
     ROI_color	: 0}
end

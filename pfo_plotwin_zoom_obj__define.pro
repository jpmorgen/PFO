;+
; NAME: pfo_plotwin_zoom_obj
;
; PURPOSE: Creates an object which allows the mouse to zoom/unzoom the
; plot in the plotwin to which this object is connected
;
; CATEGORY: PFO widgets
;
; CALLING SEQUENCE:
;
; DESCRIPTION:
;
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
; $Id: pfo_plotwin_zoom_obj__define.pro,v 1.1 2012/01/26 16:21:41 jpmorgen Exp $
;
; $Log: pfo_plotwin_zoom_obj__define.pro,v $
; Revision 1.1  2012/01/26 16:21:41  jpmorgen
; Initial revision
;
;-

function pfo_plotwin_zoom_obj::plotwin_event, event
  ;; We will always swallow the event
  retval = !tok.nowhere

  ;; Check to make sure we were forwarded the proper event.
  sn = tag_names(event, /structure_name)
  if sn ne 'WIDGET_DRAW' then $
     message, 'ERROR: expecting a WIDGET_DRAW event, got ' + sn

  ;; Check to make sure we have a plot
  plot_obj = self.plotwin_obj->last_pfo_plot_obj()
  if NOT obj_valid(plot_obj) then $
     return, retval

  ;; Right mouse button unzooms
  if event.press eq !tok.right then begin
     ;; Let the plot_obj do all the work
     plot_obj->unzoom
  endif

  return, retval

end

;; Callback routine when tROI is set
pro pfo_plotwin_zoom_obj::set_zoom, tROI
  ;; Save some typing
  plot_obj = self.plotwin_obj->last_pfo_plot_obj()
  ;; Save our current plot ranges in the plot_obj zoom list
  plot_obj->save_zoom
  ;; Set our new Xin_range to the tROI
  plot_obj->set_property, plot_Xin_range=tROI
end

;; Cleanup method
pro pfo_plotwin_zoom_obj::cleanup
  ;; Call our inherited cleaup routines
  self->pfo_tROI_obj::cleanup
end

function pfo_plotwin_zoom_obj::init, $
   _REF_EXTRA=extra ;; All input parameters, including pfo_obj, 
  ;; are passed to the init method and underlying routines via
  ;; _REF_EXTRA mechanism

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

  ;; Call our inherited init routines.  This registers our plotwin_obj
  ok = self->pfo_tROI_obj::init(tROI_set_proc='set_zoom', tROI_calling_obj=self, _EXTRA=extra)
  if NOT ok then return, 0

  ;; Register our event handler.  The pfo_tROI_obj takes care of the
  ;; left button and mouse motion for defining the tROI, but we want
  ;; to capture 
  self.plotwin_obj->register_forward, $
     {method:'plotwin_event', obj:self}, /draw_button_events

  ;; If we made it here, we have successfully set up our widget.
  return, 1

end

pro pfo_plotwin_zoom_obj__define

  ;; Make sure our system variables are defined for all of the
  ;; routines in this file
  init = {tok_sysvar}
  init = {pfo_sysvar}

  objectClass = $
     {pfo_plotwin_zoom_obj, $
      inherits pfo_tROI_obj $ ;; This inherits pfo_plotwin_cw_obj
     }
end

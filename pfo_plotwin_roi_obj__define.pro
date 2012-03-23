;+
; NAME: pfo_plotwin_roi_obj

; PURPOSE: Creates an object which allows the mouse to create regions
; of interest (ROIs).  Once you create the object, don't forget to
; call the register_forward method _of this object_ to properly
; connect to the stream of events coming from the plotwin_obj's draw
; widget

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
; $Id: pfo_plotwin_roi_obj__define.pro,v 1.1 2012/03/23 01:23:38 jpmorgen Exp $
;
; $Log: pfo_plotwin_roi_obj__define.pro,v $
; Revision 1.1  2012/03/23 01:23:38  jpmorgen
; Initial revision
;
;
;-

;; button name to use in drop-down menus or button bases.  Allows
;; easy management of many mouse modes in a mouse mode chooser widget
;; (e.g. pfo_mouse_mode_menu)
function pfo_plotwin_roi_obj::button_name
  return, 'ROI'
end

;; Event handler for events coming from the draw widget of the
;; plotwin_obj to which we are connected
function pfo_plotwin_roi_obj::plotwin_event, event

  ;; Pass the event on to our tROI first, which does some basic error
  ;; checking and handles the tROI stuff.
  OK = self->tROI_plotwin_event(event)

  ;; We will always swallow the event
  retval = !tok.nowhere

  return, retval

end

;; Callback routine when tROI is set
pro pfo_plotwin_roi_obj::set_ROI, tROI
  ;; Get the pfo_obj that our plotwin is displaying
  pfo_obj = self.plotwin_obj->last_pfo_obj()
  ;; Save our state so that we can undo
  pfo_obj->save_undo
  ;; Create our new ROI function
  pfo_obj->append_parinfo, 'pfo_ROI', ROI=tROI
end

;; Register our event with the the plot_obj event fowarding system.
;; This is a disruptive event handler: mouse clicks may interfere with
;; other mouse click objects, so we need to register with a changeable
;; event handler.
pro pfo_plotwin_roi_obj::register_forward, $
   event_forward_obj=event_forward_obj ;; (optional) usually we are installed into the default event_forward_obj in the plotwin_obj 

  if NOT obj_valid(self.plotwin_obj) then $
     message, 'ERROR: plotwin_obj is not yet registered with this obj.  Use set_property, plotwin_obj=plotwin_obj or pass plotwin_obj when initializing this object'

  self.plotwin_obj->register_forward, $
     /changeable, event_forward_obj=event_forward_obj, $
     {method:'plotwin_event', obj:self}, /draw_motion_events, /draw_button_events

widget_control
end

;; Cleanup method
pro pfo_plotwin_roi_obj::cleanup
  ;; Call our inherited cleaup routines
  self->pfo_tROI_obj::cleanup
end

function pfo_plotwin_roi_obj::init, $
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
  
  ;; Call our inherited init routines.  This sets the callback routine
  ;; in pfo_tROI_obj to our local set_ROI method.  In other words,
  ;; when we are done defining the tROI, set_ROI is called.  Note
  ;; that plotwin_obj is passed via _EXTRA
  ok = self->pfo_tROI_obj::init(tROI_set_proc='set_ROI', tROI_calling_obj=self, _EXTRA=extra)
  if NOT ok then return, 0
  
  ;; Don't register ourselves with the forward when we are first set
  ;; up, since zoom probably wants to be there

  ;; If we made it here, we have successfully set up our widget.
  return, 1

end

pro pfo_plotwin_roi_obj__define

  ;; Make sure our system variables are defined for all of the
  ;; routines in this file
  init = {tok_sysvar}
  init = {pfo_sysvar}

  objectClass = $
     {pfo_plotwin_roi_obj, $
      inherits pfo_tROI_obj $ ;; This inherits pfo_plotwin_cw_obj
     }
end

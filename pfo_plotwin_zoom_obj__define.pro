;+
; NAME: pfo_plotwin_zoom_obj
;
; PURPOSE: Creates an object which allows the mouse to zoom/unzoom the
; plot in the plotwin to which this object is connected.  Once you
; create the object, don't forget to call the register_forward
; method _of this object_ to properly connect to the stream of events
; coming from the plotwin_obj's draw widget
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
; $Id: pfo_plotwin_zoom_obj__define.pro,v 1.2 2012/03/23 01:46:51 jpmorgen Exp $
;
; $Log: pfo_plotwin_zoom_obj__define.pro,v $
; Revision 1.2  2012/03/23 01:46:51  jpmorgen
; Connect to tROI in a more object-oriented way
;
; Revision 1.1  2012/01/26 16:21:41  jpmorgen
; Initial revision
;
;-

;; button name to use in drop-down menus or button bases.  Allows
;; easy management of many mouse modes in a mouse mode chooser widget
;; (e.g. pfo_mouse_mode_menu)
function pfo_plotwin_zoom_obj::button_name
  return, 'zoom'
end

;; Event handler for events coming from the draw widget of the
;; plotwin_obj to which we are connected
function pfo_plotwin_zoom_obj::plotwin_event, event

  ;; Pass the event on to our tROI first, which does some basic error
  ;; checking and handles the tROI stuff.
  OK = self->tROI_plotwin_event(event)

  ;; We will always swallow the event
  retval = !tok.nowhere

  ;; Right mouse button unzooms
  if event.press eq !tok.right then begin
     ;; Let the plot_obj do all the work
     plot_obj = self.plotwin_obj->last_pfo_plot_obj()
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

;; Register our event with the the plot_obj event fowarding system.
;; This is a disruptive event handler: mouse clicks may interfere with
;; other mouse click objects, so we need to register with a changeable
;; event handler.
pro pfo_plotwin_zoom_obj::register_forward, $
   event_forward_obj=event_forward_obj ;; (optional) usually we are installed into the default event_forward_obj in the plotwin_obj 

  if NOT obj_valid(self.plotwin_obj) then $
     message, 'ERROR: plotwin_obj is not yet registered with this obj.  Use set_property, plotwin_obj=plotwin_obj or pass plotwin_obj when initializing this object'

  self.plotwin_obj->register_forward, $
     /changeable, event_forward_obj=event_forward_obj, $
     {method:'plotwin_event', obj:self}, /draw_motion_events, /draw_button_events

widget_control
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
  
  ;; Call our inherited init routines.  This sets the callback routine
  ;; in pfo_tROI_obj to our local set_zoom method.  In other words,
  ;; when we are done defining the tROI, set_zoom is called.  Note
  ;; that plotwin_obj is passed via _EXTRA
  ok = self->pfo_tROI_obj::init(tROI_set_proc='set_zoom', tROI_calling_obj=self, _EXTRA=extra)
  if NOT ok then return, 0
  
  ;; If we have been given a plotwin_obj, go ahead and register
  ;; ourselves in its changeable event handler, since zoom is pretty
  ;; much the most basic mouse function
  if obj_valid(self.plotwin_obj) then $
     self->register_forward
  

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

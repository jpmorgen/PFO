;+
; NAME: pfo_event_forward_obj

; PURPOSE: provide a re-configurable (changeable) object-oriented
; event handler.  Expected use is for plotwin events.  If your event
; can always listen to the stream of events from the plotwin without
; conflicting with other event handlers (e.g. pfo_cursor_cw), don't
; use this obj, just register it directly with the plotwin_obj as
; /persistent.  If your event handler would conflict with another
; (such as pfo_plotwin_zoom_obj and objs that handle ROI and peak
; definition), this is the object for you.  The /changeable option in
; the register_forward method of pfo_plotwin_obj invokes this object.

; CATEGORY: PFO event handling/widgets

; CALLING SEQUENCE: By default, this is inherited once into the
; plotwin obj once, since that is probably all it will be needed.
; Multiple instances can be created and registered with the
; plotwin_obj as many times as desired to control other event
; decisions.

; DESCRIPTION: This object sets up an object-oriented event that acts
; like a traffic cop.  Using the event_foward method, register your
; object-oriented event handler.  The next call to the event_forward
; method replaces your event handler with another.

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
; $Id: pfo_event_forward_obj__define.pro,v 1.1 2012/03/23 01:46:00 jpmorgen Exp $
;
; $Log: pfo_event_forward_obj__define.pro,v $
; Revision 1.1  2012/03/23 01:46:00  jpmorgen
; Initial revision
;
;-

;; This is the reconfigurable event handler that is registered with a
;; pfo_plotwin (usually self, by inheritance, hence the long names in
;; this object).  It basically just looks up the current event handler
;; in the property and calls that 
function pfo_event_forward_obj::event_forward_handler, event

  ;; We will always swallow the event
  retval = !tok.nowhere

  ;; Check to see if there is any event registered
  if N_elements(*self.pchangable_event) eq 0 then $
     return, retval

  ;; If we made it here, we hopefully have a properly formatted
  ;; forward event.

  ;; Catch errors, which are presumably from improperly formatted
  ;; event structures.
  ;; Handle pfo_debug level.  CATCH errors if _not_ debugging
  if !pfo.debug le 0 then begin
     CATCH, err
     if err ne 0 then begin
        CATCH, /CANCEL
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'ERROR: caught the above error.  Did you register your forward entry properly?  Should have been registered as {method:method, obj:cw_obj, [keywords:{keywords}]}.', /CONTINUE
        return, retval
     endif
  endif ;; not debugging

  ;; Call our method
  eh = *self.pchangable_event
  junk = where(tag_names(eh) eq 'KEYWORDS', count)
  if count eq 0 then $
     return, call_method(eh.method, eh.obj, event) $
  else $
     return, call_method(eh.method, eh.obj, event, _EXTRA=eh.keywords)

  return, retval

end


;; The register_event_forward_obj method is invoked when you want to
;; have this object register itself with a pfo_plotwin_obj.  It does
;; the registration and puts a copy of the plotwin_obj it is
;; registered with into the local object property so that stuff like
;; the drawID can be pulled up when necessary
pro pfo_event_forward_obj::register_event_forward_obj, $
   plotwin_obj_in ;; (optional) plotwin_obj in which to register this event forwarder.  Assumed to be self (a plotwin_obj) if omitted

  ;; Do the dance so as to not clobber or unexpectedly define our
  ;; input variable.  Usually, the plotwin_obj will be ourself, since
  ;; we get inherited into the plotwin_obj
  plotwin_obj = self
  if N_elements(plotwin_obj_in) ne 0 then $
     plotwin_obj = plotwin_obj_in

  ;; Register our event handler in the plotwin_obj
  plotwin_obj->register_forward, /persistent, $
     {method:'event_forward_handler', obj:self}

  ;; Store the plotwin_obj in our property
  self.plotwin_obj = plotwin_obj  
  
end

;; The event_change method is a one-stop shop that takes care of
;; registering, clearing, and returning the the event that is
;; being forwarded
pro pfo_event_forward_obj::event_change, $
   eh, $ ;; (optional) event handler to forward to
   clear=clear, $ ;; clears current event
   return_event=return_event, $ ;; returns current event
   _EXTRA=widget_control_args ;; args to pass to widget_control of draw widget so proper events are generated

  ;; Do a sanity check so user doesn't set and erase at the same time
  if N_elements(eh) ne 0 and keyword_set(clear) then $
     message, 'ERROR: specify either event handler (eh) or /clear'

  ;; Register a forwarded event
  if N_elements(eh) ne 0 then begin
     ;; Be militant about how events are defined (see EVENTS
     ;; documentation in pfo_cw_obj__define.pro)
     if size(/type, eh) ne !tok.struct then $
     message, 'ERROR: event handler must be a struct of the form {method:method, obj:cw_obj, [keyowrds:{keywords}]}'

     ;; If we made it here, we are good to store the event
     *self.pchangable_event = eh
     
     ;; Check to see if we need to turn on any event generation
     ;; in the plotwin_obj's draw widget.  First check to see
     ;; if we have a plotwin_obj yet
     if obj_valid(self.plotwin_obj) and keyword_set(widget_control_args) then begin
        widget_control, self.plotwin_obj->drawID(), _EXTRA=widget_control_args
     endif ;; we have a plotwin_obj and want to tweak its drawID events

  endif ;; registering event

  if keyword_set(clear) then $
     *self.pchangable_event = ptr_new(/allocate)

  ;; return event
  if keyword_set(return_event) or arg_present(return_event) then $
     return_event = *self.pchangable_event
end

;; Cleanup method
pro pfo_event_forward_obj::cleanup
  ptr_free, self.pchangable_event
  ptr_free, self.pwidget_control_args
end

;; Init method
function pfo_event_forward_obj::init, $
   event, $ ;; (optional) initial event to store in our property
   _EXRTA=extra ;; passed to event_change method (args to widget_control of plotwin_obj draw widget)

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

  ;; Initialize our pointers
  self.pchangable_event = ptr_new(/allocate)
  self.pwidget_control_args = ptr_new(/allocate)

  ;; Use our local event_change routine to store event, if provided
  self->pfo_event_forward_obj::event_change, event, _EXTRA=extra

  ;; If we made it here, we have successfully set up our object
  return, 1

end

;; Object class definition
pro pfo_event_forward_obj__define
  ;; Initialize system variables for all methods
  init = {tok_sysvar}
  init = {pfo_sysvar}

  objectClass = $
     {pfo_event_forward_obj, $
      pchangable_event	: ptr_new(), $ ;; pointer structure which defines event to foward to
      pwidget_control_args: ptr_new(), $ ;; pointer to args to pass to widget_control of draw widget so proper events are generated
      plotwin_obj: obj_new() $ ;; stores plotwin_obj we are registered with
     }
end

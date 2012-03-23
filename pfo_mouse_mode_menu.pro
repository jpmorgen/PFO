;+
; NAME: pfo_mouse_mode_menu
;
; PURPOSE: create a menu within the pfo cw_obj system to allow user to
; enable undo, undo, redo, and erase all.
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
; $Id: pfo_mouse_mode_menu.pro,v 1.1 2012/03/23 01:23:05 jpmorgen Exp $
;
; $Log: pfo_mouse_mode_menu.pro,v $
; Revision 1.1  2012/03/23 01:23:05  jpmorgen
; Initial revision
;
;
;-

function pfo_mouse_mode_menu_obj::event, event, button=button

  sn = tag_names(event, /structure_name)
  if sn ne 'WIDGET_BUTTON' then $
     message, 'ERROR: unexpected event type ' + sn

  if N_elements(button) eq 0 then $
     message, 'ERROR: button keyword required'

  ;; Register the event handler of the selected menu item in our
  ;; event_forward_obj
  (*self.pmouse_obj_list)[button]->register_forward, event_forward_obj=self.event_forward_obj

  ;; Refresh our button check boxes
  self->refresh

  ;; Swallow event
  return, !tok.nowhere

end

;; Refresh method.  pfo_cw_obj handles error catching
pro pfo_mouse_mode_menu_obj::refresh
  ;; Handle pfo_debug level.  CATCH errors if _not_ debugging
  if !pfo.debug le 0 then begin
     CATCH, err
     if err ne 0 then begin
        CATCH, /CANCEL
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'ERROR: caught the above error.  Not refreshing mouse mode menu.', /CONTINUE
        return
     endif
  endif ;; not debugging

  ;; I don't think you can get an exclusive base with a menu
  ;; button, so do it by hand.

  ;; Clear our all our buttons
  for ib=0, N_elements(*self.pIDs)-1 do begin
     widget_control, (*self.pIDs)[ib], set_button=0
  endfor ;; clearing buttons

  ;; Get the current obj that our event_forward_obj is forwarding
  ;; events to
  self.event_forward_obj->event_change, return_event=eh
  ;; Quietly return if there is no event handler yet
  if N_elements(eh) eq 0 then $
     return
  ;; Do a sanity check on our event handler
  if size(/type, eh) ne !tok.struct then $
     message, 'ERROR: event must be a struct of the form {method:method, obj:cw_obj, [keyowrds:{keywords}]}'
  ;; See if this eh was properly registered
  oidx = where(eh.obj eq *self.pmouse_obj_list, count)
  if count eq 0 then $
     message, 'ERROR: event handler in changeable event not properly registered'

  ;; If we made it here, we know which one of our mouse control objs
  ;; is active

  ;; Select the check box for our active eh
  widget_control, (*self.pIDs)[oidx], /set_button

end

pro pfo_mouse_mode_menu_obj::populate, $
   _REF_EXTRA=extra ;; for now, swallow any extra keywords

  ;; Make a button for each of our objects
  for imo=0, N_elements(*self.pmouse_obj_list)-1 do begin
     ;; Handle pfo_debug level.  CATCH errors if _not_ debugging
     if !pfo.debug le 0 then begin
        CATCH, err
        if err ne 0 then begin
           CATCH, /CANCEL
           message, /NONAME, !error_state.msg, /CONTINUE
           message, 'ERROR: caught the above error.  Skipping this mouse control object.', /CONTINUE
           CONTINUE
        endif
     endif ;; not debugging
     ;; Accumulate the widget IDs of our buttons in property.  Need to
     ;; use call_method, since IDL doesn't know that the
     ;; pmouse_obj_list is of type obj at compile time.
     button_name = call_method('button_name', (*self.pmouse_obj_list)[imo])
     pfo_array_append, *self.pIDs, $
                       widget_button(self.tlbID, $
                                     value=button_name, $
                                     uvalue={method: 'event', obj:self, keywords:{button:imo}}, $
                                     /checked_menu)
  endfor ;; each managed mouse mode

  ;; Use the refresh method to set our checkbox to the proper state
  self->refresh

end

;; Cleanup method
pro pfo_mouse_mode_menu_obj::cleanup
  ;; Call our inherited cleaup routines
  self->pfo_cw_obj::cleanup
  ;; Clean up mouse control objects, if we created them
  if keyword_set(self.created_mouse_objs) then begin
     for imo=0, N_elements(*self.pmouse_obj_list)-1 do begin
        obj_destroy, (*self.pmouse_obj_list)[imo]
     endfor
  endif ;; cleaning up our created mouse objects
  ;; Clean up our local pointers
  ptr_free, self.pmouse_obj_list
  ptr_free, self.pIDs
end

;; Init method.  
function pfo_mouse_mode_menu_obj::init, $
   parentID, $ ;; widgetID of parent widget
   mouse_obj_list=mouse_obj_list_in, $ ;; (optional) list of mouse controlling objects.  Default: creates a pfo_plotwin_zoom_obj, and pfo_plotwin_roi_obj
   event_forward_obj=event_forward_obj, $ ;; (optional) changeable event handler that manages the mouse_obj_list events: default is the default changeable event hander in plotwin_obj
   plotwin_obj=plotwin_obj, $ ;; required if mouse_obj_list or event_forward_obj is not specified
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

  ;; Call our inherited init routine.
  ok = self->pfo_cw_obj::init(parentID, /menu, value='Mouse mode', $
                              _EXTRA=extra)
  if NOT ok then return, 0

  ;; Initialize our mouse_obj_list, protecting input
  if N_elements(mouse_obj_list_in) ne 0 then $
     mouse_obj_list = mouse_obj_list_in
  ;; Create basic mouse control objects if none specified
  if N_elements(mouse_obj_list) eq 0 then begin
     if N_elements(plotwin_obj) eq 0 then $
        message, 'ERROR: plotwin_obj required so that default mouse control objects can be properly connected to the plotwin'
     ;; Tue Mar 20 14:59:31 2012  jpmorgen@snipe
     ;; So far, we only have two mouse control objects: zoom and ROI
     mouse_obj_list = $
        [obj_new('pfo_plotwin_zoom_obj', plotwin_obj=plotwin_obj, event_forward_obj=event_forward_obj), $
         obj_new('pfo_plotwin_ROI_obj', plotwin_obj=plotwin_obj, event_forward_obj=event_forward_obj)]
     self.created_mouse_objs = 1
  endif

  ;; Put the mouse_obj_list into our local property
  self.pmouse_obj_list = ptr_new(mouse_obj_list)

  ;; Put our changeable event handler in our local property.  Note
  ;; event_forward_obj takes precedence over plotwin_obj, since
  ;; plotwin_obj, by default, contains an event forwarder, but we
  ;; might want
  if obj_valid(plotwin_obj) then $
     self.event_forward_obj = plotwin_obj
  if obj_valid(event_forward_obj) then $
     self.event_forward_obj = event_forward_obj
  if NOT obj_valid(self.event_forward_obj) then $
     message, 'ERROR: specify a valid event_forward_obj or pfo_plotwin_obj'

  ;; Initialize other local pointers
  self.pIDs = ptr_new(/allocate)

  ;; Build our widget
  self->populate, _EXTRA=extra

  ;; If build is sucessful, we can register ourselves in the refresh
  ;; list
  self->register_refresh

  ;; If we made it here, we have successfully set up our container.  
  return, 1

end

;; Object class definition.
pro pfo_mouse_mode_menu_obj__define
  objectClass = $
     {pfo_mouse_mode_menu_obj, $
      pmouse_obj_list		: ptr_new(), $ ;; list of mouse controlling obj_refs
      pIDs			: ptr_new(), $ ;; list of menu widget IDs
      created_mouse_objs	: 0B, $ ;; flag to indicate if we created our mouse controllers locally
      event_forward_obj		: obj_new(), $ ;; pfo_event_forward_obj that manages the changeable event handler we tweak when we change mouse modes
      inherits pfo_cw_obj}
end

function pfo_mouse_mode_menu, $
   parentID, $ ;; Parent widget ID (positional parameter)
   cw_obj=cw_obj, $ ;; (output) the object that runs this cw
   _REF_EXTRA=extra ;; ;; All other input parameters passed to the init method and underlying routines via _REF_EXTRA mechanism

  ;; Make sure our system variables are defined for all of the
  ;; routines in this file
  init = {tok_sysvar}
  init = {pfo_sysvar}

  ;; Initialize output
  cwID = !tok.nowhere

  ;; Create our controlling object
  cw_obj = pfo_cw_obj_new(parentID, _EXTRA=extra)

  ;; The init method creates the widget and stores its ID in self.tlb.
  ;; Use the getID method to access it.  We return this ID, since that
  ;; is what people expect when they call a widget creation function.
  ;; What people will probably really want is the object to do the
  ;; heavy-duty control.  Default to a nonsense widgetID unless the
  ;; object creation was sucessful.
  if obj_valid(cw_obj) then begin
     cwID = cw_obj->tlbID()
  endif ;; valid cw_obj

  return, cwID

end

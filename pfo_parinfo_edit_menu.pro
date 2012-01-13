;+
; NAME: pfo_parinfo_edit_menu
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
; $Id: pfo_parinfo_edit_menu.pro,v 1.3 2012/01/13 21:02:02 jpmorgen Exp $
;
; $Log: pfo_parinfo_edit_menu.pro,v $
; Revision 1.3  2012/01/13 21:02:02  jpmorgen
; Make token-based logic
;
; Revision 1.2  2011/12/01 22:16:48  jpmorgen
; Make string tokens out of actions rather than numbers
;
; Revision 1.1  2011/09/23 13:10:31  jpmorgen
; Initial revision
;
;-

function pfo_parinfo_edit_menu_obj::event, event, button=button

  sn = tag_names(event, /structure_name)
  if sn ne 'WIDGET_BUTTON' then $
     message, 'ERROR: unexpected event type ' + sn

  if N_elements(button) eq 0 then $
     message, 'ERROR: button keyword required'

  case button of 
     self.enable_undo: begin ;; toggle enable_undo.
        self.pfo_obj->get_property, enable_undo=enable_undo
        self.pfo_obj->set_property, enable_undo=~enable_undo
        ;; Refresh any other instances that display this information
        self.pfo_obj->refresh
     end
     ;; Undo and redo take care of update stuff
     self.undo: self.pfo_obj->undo
     self.redo: self.pfo_obj->redo
     self.erase: begin
        ok = dialog_message('Erase the entire parinfo?!', title='Erase?', /question, /default_no, dialog_parent=self.parentID)
        if ok eq 'Yes' then begin
           self.pfo_obj->delete_parinfo
           self.pfo_obj->update
        endif
     end
     else : message, 'ERROR: unrecognized button token: ' + button
  endcase

  ;; Swallow event
  return, !tok.nowhere

end

;; Refresh method.  pfo_cw_obj handles error catching
pro pfo_parinfo_edit_menu_obj::refresh
  ;; Get the property we display
  self.pfo_obj->get_property, enable_undo=enable_undo
  ;; Set the checkbox
  widget_control, self.enable_undoID, set_button=enable_undo
  
end

pro pfo_parinfo_edit_menu_obj::populate, $
   _REF_EXTRA=extra ;; for now, swallow any extra keywords

  self.enable_undoID = widget_button(self.tlbID, value='Enable undo', /checked_menu, $
                                     uvalue={method: 'event', obj:self, keywords:{button:self.enable_undo}})
  ID = widget_button(self.tlbID, value='Undo', $
                     uvalue={method: 'event', obj:self, keywords:{button:self.undo}})
  ID = widget_button(self.tlbID, value='Redo', $
                     uvalue={method: 'event', obj:self, keywords:{button:self.redo}})
  ID = widget_button(self.tlbID, value='Erase all', $
                     uvalue={method: 'event', obj:self, keywords:{button:self.erase}})
  ;; --> It would be nice to have an "add function" item here, but I
  ;; can't just use pfo_parinfo_new_droplist, since a droplist can't
  ;; be the child of a button menu.  I would have to make separate
  ;; buttons for each function.  Since the parinfo display window is
  ;; likely to be up and have the droplist, put this feature off for
  ;; now....

  ;; Use the refresh method to set our checkbox to the proper state
  self->refresh

end

;; Cleanup method
pro pfo_parinfo_edit_menu_obj::cleanup
  ;; Call our inherited cleaup routines
  self->pfo_cw_obj::cleanup
  ;; Nothing local to do
end

;; Init method
function pfo_parinfo_edit_menu_obj::init, $
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

  ;; Call our inherited init routine.
  ok = self->pfo_cw_obj::init(parentID, /menu, value='Edit', _EXTRA=extra)
  if NOT ok then return, 0

  ;; Initialize our tokens
  self.enable_undo = 0
  self.undo = 1
  self.redo = 2
  self.erase = 3

  ;; Build our widget
  self->populate, _EXTRA=extra

  ;; If build is sucessful, we can register ourselves in the refresh
  ;; list
  self->register_refresh

  ;; If we made it here, we have successfully set up our container.  
  return, 1

end

;; Object class deparinfo_edit_menuion.  This widget operates on general pfo_obj
;; property, NOT the parinfo, so it can be a plain pfo_cw_obj
pro pfo_parinfo_edit_menu_obj__define
  objectClass = $
     {pfo_parinfo_edit_menu_obj, $
      enable_undoID	: 0L, $ ;; widget ID of child that does all the work
      enable_undo	: 0B, $ ;; tokens for event
      undo		: 0B, $
      redo		: 0B, $
      erase		: 0B, $
      inherits pfo_cw_obj}
end

function pfo_parinfo_edit_menu, $
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

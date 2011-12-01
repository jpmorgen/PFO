;+
; NAME: pfo_parinfo_new_exit_menu

; PURPOSE: create a simple menu containing 'Done' and 'Cancel' which
; gives the user two choices when editing a new parinfo segment with a
; top-level base pfo_parinfo_edit window.  'Done' inserts the edited
; segment into the calling routines pfo_obj, 'Cancel' discards the new
; parinfo and leavfes th eoriginal parinfo/pfo_obj unchanged.

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
; $Id: pfo_parinfo_new_exit_menu.pro,v 1.2 2011/12/01 22:15:43 jpmorgen Exp $
;
; $Log: pfo_parinfo_new_exit_menu.pro,v $
; Revision 1.2  2011/12/01 22:15:43  jpmorgen
; Minor doc fix
;
; Revision 1.1  2011/09/23 13:05:55  jpmorgen
; Initial revision
;
;-

;; Cancel means discard what we have done
function pfo_parinfo_new_exit_menu_obj::cancel, event

  sn = tag_names(event, /structure_name)
  if sn ne 'WIDGET_BUTTON' then $
     message, 'ERROR: unexpected event type ' + sn

  ;; We need to kill the temporary pfo_obj to prevent memory leaks.
  ;; This also kills the widget displaying the pfo_obj
  obj_destroy, self.pfo_obj

  ;; Swallow this event
  return, !tok.nowhere
end

;; Done means append our new parinfo to the orig_pfo_obj
function pfo_parinfo_new_exit_menu_obj::done, event

  sn = tag_names(event, /structure_name)
  if sn ne 'WIDGET_BUTTON' then $
     message, 'ERROR: unexpected event type ' + sn

  ;; We should have a parinfo in our (new) pfo_obj, but check just in case
  N_parinfo = self.pfo_obj->parinfo_call_function(/no_update, 'N_elements')
  if N_parinfo ne 0 then begin
     ;; Append the newly created and user-initialized parinfo to the
     ;; orig_pfo_obj parinfo.  This handles all of the updates, undos, etc.
     self.orig_pfo_obj->parinfo_call_procedure, $
        /save_undo, 'pfo_array_append', self.pfo_obj->parinfo(/no_copy)
  endif ;; appending new parinfo
  
  ;; We need to kill the temporary pfo_obj to prevent memory leaks.
  ;; This also kills the widget displaying the pfo_obj
  obj_destroy, self.pfo_obj

  ;; Swallow this event
  return, !tok.nowhere

end


pro pfo_parinfo_new_exit_menu_obj::populate, $
   _REF_EXTRA=extra ;; for now, swallow any extra keywords

  ;; Done means append our new parinfo to the orig_pfo_obj
  ID = widget_button(self.tlbID, value='Done', $
                        uvalue={method: 'done', obj:self})
  ;; Cancel button deletes parinfo as a hint to the calling routine
  ;; that the user doesn't want to use it.
  ID = widget_button(self.tlbID, value='Cancel', $
                        uvalue={method: 'cancel', obj:self})

end

;; Cleanup method
pro pfo_parinfo_new_exit_menu_obj::cleanup
  ;; Call our inherited cleaup routines
  self->pfo_cw_obj::cleanup
  ;; Nothing local to do
end

;; Init method
function pfo_parinfo_new_exit_menu_obj::init, $
   parentID, $ ;; widgetID of parent widget
   orig_pfo_obj=orig_pfo_obj, $ ;; pfo_obj into which new function will be inserted (if desired)
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
  ok = self->pfo_cw_obj::init(parentID, /menu, value='Menu', _EXTRA=extra)
  if NOT ok then return, 0

  ;; Store the orig_pfo_obj for use by our 'Done' button
  if N_elements(orig_pfo_obj) ne 0 then $
     self.orig_pfo_obj = orig_pfo_obj

  ;; Build our widget
  self->populate, _EXTRA=extra

  ;; If we made it here, we have successfully set up our container.  
  return, 1

end

;; Object class definition.  This widget operates on general pfo_obj
;; property, NOT the parinfo, so it can be a plain pfo_cw_obj
pro pfo_parinfo_new_exit_menu_obj__define
  objectClass = $
     {pfo_parinfo_new_exit_menu_obj, $
      orig_pfo_obj	: obj_new(), $ ;; pfo_obj into which new function will be inserted (if desired)
      inherits pfo_cw_obj}
end

function pfo_parinfo_new_exit_menu, $
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

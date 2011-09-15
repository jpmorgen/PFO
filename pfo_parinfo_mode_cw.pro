;+
; NAME: pfo_parinfo_mode_cw
;
; PURPOSE: display active, inactive, delete, and (optionally) edit
; radio buttons for an instance of a function.  See pfo_null__widget
; for an example
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
; $Id: pfo_parinfo_mode_cw.pro,v 1.1 2011/09/15 20:51:09 jpmorgen Exp $
;
; $Log: pfo_parinfo_mode_cw.pro,v $
; Revision 1.1  2011/09/15 20:51:09  jpmorgen
; Initial revision
;
;-

function pfo_parinfo_mode_cw_obj::event, event, action=action

  ;; We will always swallow the event
  retval = !tok.nowhere

  ;; Check to see if we have changed our status
  self.pfo_obj->parinfo_call_procedure, $
     /no_update, 'pfo_struct_setget_tag', /get, idx=*self.pidx, $
     taglist_series='pfo', status=status

  ;; Make sure we have a unique status value in our parinfo
  if N_elements(uniq(status, sort(status))) ne 1 then $
     message, 'ERROR: non-unique pfo.status tag for this function.'
  status = status[0]

  if action eq status then $
     return, retval

  ;; Handle the delete case
  if action eq !pfo.delete then begin
     ;; Raise a widget making sure that the user wants to do this
     ok = dialog_message('Are you sure that you want to permanently delete this sub-function?', title='Delete?', /question, /default_no, dialog_parent=self.parentID)
     ;; We could leave it marked as delete, but then we would need
     ;; another button for garbage collecting, which might be
     ;; confusing.  For now, always gc if the user wants to delete
     if ok eq 'No' then begin
       self->refresh
        return, retval
     endif ;; not garbage collecting
  endif ;; delete

  case action of 
     !pfo.active : self.pfo_obj->parinfo_call_procedure, $
        'pfo_mode', 'active', idx=*self.pidx
     !pfo.inactive : self.pfo_obj->parinfo_call_procedure, $
        'pfo_mode', 'inactive', idx=*self.pidx
     !pfo.delete : begin 
        ;; Prepare to do the update
        self.pfo_obj->prepare_update, undo
        self.pfo_obj->parinfo_call_procedure, $
           /no_update, 'pfo_mode', 'delete', idx=*self.pidx
        self.pfo_obj->parinfo_call_procedure, $
           /no_update, 'pfo_gc'
        ;; Do the update
        self.pfo_obj->update, undo
     end
     !pfo.all_status : begin ;; borrowed token for edit
        topID = pfo_widget_top(self.parentID)
        ID = self.pfo_obj->parinfo_call_function( $
             /no_update, 'pfo_parinfo_parse', /widget, /edit, status_mask=!pfo.all_status, $
             title='PFO PARINFO SUB-FUNCTION EDITOR', idx=*self.pidx, /no_finit, group_leader=topID, pfo_obj=self.pfo_obj)
        ;; Put flag back to where it belongs
        self->refresh
     end
  endcase


  return, retval

end

;; Refresh method.  pfo_cw_obj handles error catching
pro pfo_parinfo_mode_cw_obj::refresh, $
   params=params

  ;; Refresh our display

  ;; Read our value from the parinfo
  self.pfo_obj->parinfo_call_procedure, $
     /no_update, 'pfo_struct_setget_tag', /get, idx=*self.pidx, $
     taglist_series='pfo', status=status

  ;; Make sure we have a unique status value in our parinfo
  if N_elements(uniq(status, sort(status))) ne 1 then $
     message, 'ERROR: non-unique pfo.status tag for this function.'
  status = status[0]

  case status of 
     !pfo.active : ID = 0
     !pfo.inactive : ID = 1
     !pfo.delete : ID = 2
     !pfo.edit : ID = 3
  endcase

  ;; Let calling routines do error catching
  widget_control, self.ID[ID], /set_button

end

pro pfo_parinfo_mode_cw_obj::populate, $
   edit=edit, $
   _REF_EXTRA=extra ;; for now, swallow any extra keywords

  ;; Have our buttons generate events only when they are pressed.
  ;; Otherwise, the exclusive base makes the button that was
  ;; previously pressed generate an event
  self.ID[0] = widget_button(self.containerID, value='active', /no_release, $
                         uvalue={method:'event', obj:self, keywords:{action:!pfo.active}})
  self.ID[1] = widget_button(self.containerID, value='inactive', /no_release, $
                         uvalue={method:'event', obj:self, keywords:{action:!pfo.inactive}})
  self.ID[2] = widget_button(self.containerID, value='delete', /no_release, $
                         uvalue={method:'event', obj:self, keywords:{action:!pfo.delete}})
  ;; A little confusing.  If we are editing, we don't need the
  ;; edit button.  Also, make sure we pick an action token that is
  ;; mutually exclusive of all of the other tokens
  if NOT keyword_set(edit) then $
       self.ID[3] = widget_button(self.containerID, value='edit', /no_release, $
                         uvalue={method:'event', obj:self, keywords:{action:!pfo.all_status}})

  ;; Use the refresh method to set the correct button
  self->refresh

end

;; Cleanup method
pro pfo_parinfo_mode_cw_obj::cleanup
  ;; Call our inherited cleaup routines
  self->pfo_parinfo_cw_obj::cleanup
end

;; Init method
function pfo_parinfo_mode_cw_obj::init, $
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
  ;; among other things.  It will also make a two-column exclusive
  ;; button base
  ok = self->pfo_parinfo_cw_obj::init(parentID, _EXTRA=extra)
  if NOT ok then return, 0

  ;; Register ourselves in the refresh list
  self->register_refresh

  ;; Create our container
  self->create_container, column=2, /exclusive

  ;; Build our widget
  self->populate, _EXTRA=extra

  ;; If we made it here, we have successfully set up our widget.
  return, 1

end

;; Object class definition
pro pfo_parinfo_mode_cw_obj__define
  objectClass = $
     {pfo_parinfo_mode_cw_obj, $
      ID	: make_array(4, value=0L), $ ;; widget IDs of buttons.  Extra is for edit
      inherits pfo_parinfo_cw_obj}
end

function pfo_parinfo_mode_cw, $
   parentID, $ ;; Parent widget ID (positional parameter)
   cw_obj=cw_obj, $ ;; (output) the object that runs this cw
   _REF_EXTRA=extra ;; All other input parameters, including pfo_obj, 
  ;; are passed to the init method and underlying routines via _REF_EXTRA mechanism

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

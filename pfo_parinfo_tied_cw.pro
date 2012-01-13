;+
; NAME: pfo_parinfo_tied_cw
;
; PURPOSE: allows user to edit the .tied field of the parinfo structure
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
; $Id: pfo_parinfo_tied_cw.pro,v 1.1 2012/01/13 20:53:31 jpmorgen Exp $
;
; $Log: pfo_parinfo_tied_cw.pro,v $
; Revision 1.1  2012/01/13 20:53:31  jpmorgen
; Initial revision
;
; Revision 1.2  2011/09/16 13:51:33  jpmorgen
; Fixed bug, Simplified widget hierarchy to try to speed up.
;
; Revision 1.1  2011/09/08 20:00:32  jpmorgen
; Initial revision
;
; Revision 1.1  2011/09/01 22:12:12  jpmorgen
; Initial revision
;
;-

function pfo_parinfo_tied_cw_obj::event, event

  ;; We will always swallow the event
  retval = !tok.nowhere

  ;; Check to see if we have changed our values
  self.pfo_obj->parinfo_call_procedure, $
     /no_update, 'pfo_struct_setget_tag', /get, idx=*self.pidx, $
     taglist_series='mpfit_parinfo', tied=tied

  widget_control, self.ID, get_value=w_value
 
  ;; No change means swallow the event
  if tied eq w_value then $
     return, retval

  ;; If we made it here, we have a valid change
  
  ;; Write it into the parinfo --> note: this does no error checking.
  ;; As it stands now, the first complaint the user will see is in
  ;; mpfit
  self.pfo_obj->parinfo_call_procedure, $
     /save_undo, 'pfo_struct_setget_tag', /set, idx=*self.pidx, $
     taglist_series='mpfit_parinfo', tied=w_value

  return, retval
end

;; Refresh method.  pfo_cw_obj handles error catching
pro pfo_parinfo_tied_cw_obj::refresh

  ;; Refresh our display

  ;; --> I would like to have a better way of doing dependencies
  ;; between the widgets.  For now, just cheat and check for the hand-tied
  
  ;; Read our value from the parinfo
  self.pfo_obj->parinfo_call_procedure, $
     /no_update, 'pfo_struct_setget_tag', /get, idx=*self.pidx, $
     taglist_series=['mpfit_parinfo', 'pfo_link'], $
     tied=tied, $
     link_status=link_status

  sensitive = 1
  if N_elements(link_status) ne 0 then $
     sensitive = link_status eq !pfo.hand_tied

  widget_control, self.ID, set_value=tied, sensitive=sensitive

end

pro pfo_parinfo_tied_cw_obj::populate, $
   _REF_EXTRA=extra ;; for now, swallow any extra keywords

  ;; Set up the basic widget and let refresh fill it in
  self.ID = pfo_cw_field(self.tlbID, title='', /string, $
                         /return_events, /kbrd_focus_events, $
                         value='', $
                         uvalue={method:'event', obj:self})
  self->refresh

end

;; Cleanup method
pro pfo_parinfo_tied_cw_obj::cleanup
  ;; Call our inherited cleaup routines
  self->pfo_parinfo_cw_obj::cleanup
end

;; Init method
function pfo_parinfo_tied_cw_obj::init, $
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
  ;; among other things
  ok = self->pfo_parinfo_cw_obj::init(parentID, _EXTRA=extra)
  if NOT ok then return, 0

  ;; Build our widget
  self->populate, _EXTRA=extra

  ;; If build is sucessful, we can register ourselves in the refresh
  ;; list
  self->register_refresh

  ;; If we made it here, we have successfully set up our widget.
  return, 1

end

;; Object class definition
pro pfo_parinfo_tied_cw_obj__define
  objectClass = $
     {pfo_parinfo_tied_cw_obj, $
      ID	: 0L, $ ;; widget ID of child that does all the work
      auto	: 0B, $ ;; tokens to indicate type of event
      status	: 0B, $
      inherits pfo_parinfo_cw_obj}
end

function pfo_parinfo_tied_cw, $
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

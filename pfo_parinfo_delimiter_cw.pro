;+
; NAME: pfo_parinfo_delimiter_cw
;
; PURPOSE: display the left or right delimiter from the parinfo structure
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
; $Id: pfo_parinfo_delimiter_cw.pro,v 1.2 2011/09/08 19:59:48 jpmorgen Exp $
;
; $Log: pfo_parinfo_delimiter_cw.pro,v $
; Revision 1.2  2011/09/08 19:59:48  jpmorgen
; Cleaned up/created update of widgets at pfo_parinfo_obj level
;
; Revision 1.1  2011/09/01 22:10:47  jpmorgen
; Initial revision
;
;-

;; Helper procedure to return the current delimiter and pegged strings
pro pfo_parinfo_delimiter_cw_obj::get_delimiter, params=params, delimiter=delimiter, pegged=pegged
  ;; Use delimeter function for printing to do the work
  delimiter = self.pfo_obj->parinfo_call_function( $
              /no_update, 'pfo_delimiter', self.side, params, *self.pidx)
  ;; Pick apart the answer.  Find * (if any) so that it can be removed
  ppos = stregex(delimiter, '\*')
  pegged = ''
  if ppos gt 0 then begin
     ;; Report pegged and delimeter separately
     delimiter = strmid(delimiter, 0, ppos)
     pegged = '*'
  endif ;; pegged
  ;; Clean up delim
  delimiter = strtrim(delimiter, 2)
end

function pfo_parinfo_delimiter_cw_obj::event, event

  sn = tag_names(event, /structure_name)
  if sn ne 'WIDGET_DROPLIST' then $
     message, 'ERROR: unexpected event'

  ;; We will always swallow the event
  retval = !tok.nowhere

  ;; We always list our current choice first, so if index = 0, there
  ;; is no change
  if event.index eq 0 then $
     return, retval

  ;; --> improve pfo_mode stuff.  Query the permanent status
  ;; first and raise a dialog box if we are going to change away from
  ;; that.  Want to do a local refresh when user selects same thing as
  ;; the current value

  ;; Use the event index and the order we always put the droplist in
  ;; to know what the user wants.  Make sure that we simultaneously
  ;; take care of limited and fixed, where appropriate
  case event.index of
     1:   begin
        ;; free
        ;; Write it into the parinfo, remembering to turn limited off.
        ;; We need to do two things at once here, so do our update by
        ;; hand
        self.pfo_obj->prepare_update, undo
        self.pfo_obj->parinfo_call_procedure, $
           /no_update, 'pfo_mode', 'free', idx=*self.pidx, /cancel_permanent
        self.pfo_obj->parinfo_call_procedure, $
           /no_update, 'pfo_struct_setget_tag', /set, idx=*self.pidx, $
           taglist_series='mpfit_parinfo', limited=0
        self.pfo_obj->update, undo, /save_undo
     end
     2:   begin
        ;; Limited requires us to write entire limited array.  Read
        ;; the existing one
        self.pfo_obj->parinfo_call_procedure, $
           /no_update, 'pfo_struct_setget_tag', /get, idx=*self.pidx, $
           taglist_series='mpfit_parinfo', limited=limited
        ;; Modify the side we have changed
        limited[self.side] = !tok.yes
        ;; Write it into the parinfo, remembering to turn fixed off.
        ;; We need to do two things at once here, so do our update by
        ;; hand
        self.pfo_obj->prepare_update, undo
        self.pfo_obj->parinfo_call_procedure, $
           /no_update, 'pfo_mode', 'free', idx=*self.pidx, /cancel_permanent
        self.pfo_obj->parinfo_call_procedure, $
           /no_update, 'pfo_struct_setget_tag', /set, idx=*self.pidx, $
           taglist_series='mpfit_parinfo', limited=limited
        self.pfo_obj->update, undo, /save_undo
     end
     3:   begin
        ;; fixed
        ;; Write it into the parinfo, remembering to turn limited off.
        ;; We need to do two things at once here, so do our update by
        ;; hand
        self.pfo_obj->prepare_update, undo
        self.pfo_obj->parinfo_call_procedure, $
           /no_update, 'pfo_mode', 'fixed', idx=*self.pidx, /cancel_permanent
        self.pfo_obj->parinfo_call_procedure, $
           /no_update, 'pfo_struct_setget_tag', /set, idx=*self.pidx, $
           taglist_series='mpfit_parinfo', limited=0
        self.pfo_obj->update, undo, /save_undo
     end

  endcase

  return, retval
end

;; Refresh method.  pfo_cw_obj handles error catching
pro pfo_parinfo_delimiter_cw_obj::refresh, params=params

  ;; Refresh our display

  self->get_delimiter, params=params, delimiter=delimiter, pegged=pegged
  widget_control, self.delimiterID, $
                  set_value=[delimiter, !pfo.delimiters]
  widget_control, self.peggedID, $
                  set_value=pegged

end

pro pfo_parinfo_delimiter_cw_obj::populate, $
   params=params, $ ;; unlikely to pass params for populate
   _REF_EXTRA=extra ;; for now, swallow any extra keywords

  self->get_delimiter, params=params, delimiter=delimiter, pegged=pegged

  self.delimiterID = widget_droplist( $
                     self.containerID, $
                     value=[delimiter, !pfo.delimiters], $
                     uvalue={method:'event', obj:self})
  self.peggedID = widget_label( $
                  self.containerID, $
                  value=pegged)

end

;; Cleanup method
pro pfo_parinfo_delimiter_cw_obj::cleanup
  ;; Call our inherited cleaup routines
  self->pfo_parinfo_cw_obj::cleanup
end

;; Init method
function pfo_parinfo_delimiter_cw_obj::init, $
   parentID, $ ;; widgetID of parent widget
   side=side, $ ;; determines which delimiter (left or right) will be displayed
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

  ;; Check our switches
  if N_elements(side) eq 0 then $
     message, 'ERROR: "side" keyword is missing (e.g. side=!pfo.left)'

  ;; Put our property in the object
  self.side=side

  ;; Call our inherited init routines.  This puts pfo_obj into self,
  ;; among other things
  ok = self->pfo_parinfo_cw_obj::init(parentID, _EXTRA=extra)
  if NOT ok then return, 0

  ;; Register ourselves in the refresh list.
  self->register_refresh

  ;; Create our container widget.  We need focus events to come from
  ;; it, since cw_field doesn't generate them and fsc_field doesn't
  ;; handle floating point numbers properly
  self->create_container, /row, /base_align_center

  ;; Build our widget
  self->populate, _EXTRA=extra

  ;; If we made it here, we have successfully set up our container.  
  return, 1

end

;; Object class definition
pro pfo_parinfo_delimiter_cw_obj__define
  objectClass = $
     {pfo_parinfo_delimiter_cw_obj, $
      delimiterID: 0L, $ ;; widget ID of the delimiter droplist
      peggedID	: 0L, $ ;; widget ID of the label widget indicating if value is pegged at limit
      side	: 0B, $ ;; determines which delimiter (left or right) will be displayed
      inherits pfo_parinfo_cw_obj}
end

function pfo_parinfo_delimiter_cw, $
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

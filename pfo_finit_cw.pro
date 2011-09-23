;+
; NAME: pfo_finit_cw
;
; PURPOSE: create a compound widget that displays the standard pfo
; parinfo finit sequence: X=Xin, Y=NaN and a droplist widget prompting
; the user to add a new sub-function
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
; $Id: pfo_finit_cw.pro,v 1.3 2011/09/23 13:03:34 jpmorgen Exp $
;
; $Log: pfo_finit_cw.pro,v $
; Revision 1.3  2011/09/23 13:03:34  jpmorgen
; Added "Add new function" droplist menu
;
; Revision 1.2  2011/09/16 13:53:15  jpmorgen
; Added pfo_obj->update stuff, simplified widget hierarchy to try to
; speed up
;
; Revision 1.1  2011/09/01 22:17:47  jpmorgen
; Initial revision
;
;-

function pfo_finit_cw_obj::event, event

  ;; Prepare to swallow this event
  retval = !tok.nowhere

  sn = tag_names(event, /structure_name)
  if sn ne 'WIDGET_DROPLIST' then $
     message, 'ERROR: unexpected event type ' + sn

  ;; Nothing to do if the default value is selected
  if event.index eq 0 then $
     return, retval

  ;; If we made it here, we have a change that we want to handle in
  ;; the context of our update/undo system
  self.pfo_obj->prepare_update, undo

  case event.index of
     0: ;; No change in init_Yaxis (handled above)
     1: self.pfo_obj->set_property, init_Yaxis=!values.d_NAN
     2: self.pfo_obj->set_property, init_Yaxis=0d
     else: message, 'ERROR: unrecognized event index'
  endcase

  self.pfo_obj->update, undo, /save_undo

  ;; Swallow event
  return, retval
end

;; Refresh method.  pfo_cw_obj handles error catching
pro pfo_finit_cw_obj::refresh
  ;; Get the property we display
  self.pfo_obj->get_property, init_Yaxis=init_Yaxis
  ;; Refresh our display
  s = string(format='(f3.0)', init_Yaxis)
  widget_control, self.init_YaxisID, $
                  set_value=[s, 'NaN', '0']

end

pro pfo_finit_cw_obj::populate, $
   _REF_EXTRA=extra ;; for now, swallow any extra keywords

  self.pfo_obj->get_property, init_Yaxis=init_Yaxis

  ;; Start with our standard parinfo function finit.  We only print
  ;; this once.  This works out to: "X = Xin; Y = " unless tokens have
  ;; been changed.
  finit = !pfo.axis_string[!pfo.Xaxis] + ' = ' + !pfo.axis_string[!pfo.Xin] + $
             '; ' + !pfo.axis_string[!pfo.Yaxis] + ' = '

  ID = widget_label(self.tlbID, value=finit)
  ;; Add a droplist menu to select the default Yaxis
  s = string(format='(f3.0)', init_Yaxis)
  self.init_YaxisID = widget_droplist(self.tlbID, value=[s, 'NaN', '0'], $
                                      uvalue={method: 'event', obj:self})

  ;; Add the new function droplist
  ID = pfo_parinfo_new_droplist(self.tlbID, pfo_obj=self.pfo_obj)

end

;; Cleanup method
pro pfo_finit_cw_obj::cleanup
  ;; Call our inherited cleaup routines
  self->pfo_cw_obj::cleanup
end

;; Init method
function pfo_finit_cw_obj::init, $
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

  ;; Call our inherited init routine.  Make sure we are a row base and
  ;; thing line up centered top-to-bottom
  ok = self->pfo_cw_obj::init(parentID, /row, /base_align_center, _EXTRA=extra)
  if NOT ok then return, 0

  ;; Register ourselves in the refresh list.
  self->register_refresh

  ;; Build our widget
  self->populate, _EXTRA=extra

  ;; If we made it here, we have successfully set up our container.  
  return, 1

end

;; Object class definition.  This widget operates on general pfo_obj
;; property, NOT the parinfo, so it can be a plain pfo_cw_obj
pro pfo_finit_cw_obj__define
  objectClass = $
     {pfo_finit_cw_obj, $
      init_YaxisID      : 0L, $ ;; widget ID(s) used in refresh method
      inherits pfo_cw_obj}
end

function pfo_finit_cw, $
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

;+
; NAME: pfo_fit_menu
;
; PURPOSE: create a simple menu which allows the issue the fit method
; of the pfo_obj
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
; $Id: pfo_fit_menu.pro,v 1.2 2011/12/01 22:16:11 jpmorgen Exp $
;
; $Log: pfo_fit_menu.pro,v $
; Revision 1.2  2011/12/01 22:16:11  jpmorgen
; Minor doc fix, standardize event return
;
; Revision 1.1  2011/09/22 23:45:59  jpmorgen
; Initial revision
;
;-

function pfo_fit_menu_obj::event, event
  
  sn = tag_names(event, /structure_name)
  if sn ne 'WIDGET_BUTTON' then $
     message, 'ERROR: unexpected event type ' + sn

  ;; Just call fit and let the pfo_obj property take care of all of
  ;; the rest of the stuff
  junk = self.pfo_obj->fit()

  ;; Swallow event
  return, !tok.nowhere

end

pro pfo_fit_menu_obj::populate, $
   _REF_EXTRA=extra ;; for now, swallow any extra keywords

  ID = widget_button(self.tlbID, value='Fit', $
                        uvalue={method: 'event', obj:self})

end

;; Cleanup method
pro pfo_fit_menu_obj::cleanup
  ;; Call our inherited cleaup routines
  self->pfo_cw_obj::cleanup
  ;; Nothing local to do
end

;; Init method
function pfo_fit_menu_obj::init, $
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
  ok = self->pfo_cw_obj::init(parentID, /menu, value='Fit', _EXTRA=extra)
  if NOT ok then return, 0

  ;; Build our widget
  self->populate, _EXTRA=extra

  ;; If we made it here, we have successfully set up our container.  
  return, 1

end

;; Object class definition.  This widget operates on general pfo_obj
;; property, NOT the parinfo, so it can be a plain pfo_cw_obj
pro pfo_fit_menu_obj__define
  objectClass = $
     {pfo_fit_menu_obj, $
      inherits pfo_cw_obj}
end

function pfo_fit_menu, $
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

;+
; NAME: pfo_poly_order_cw
;
; PURPOSE: display parinfo value
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
; $Id: pfo_poly_order_cw.pro,v 1.1 2011/10/25 21:31:23 jpmorgen Exp $
;
; $Log: pfo_poly_order_cw.pro,v $
; Revision 1.1  2011/10/25 21:31:23  jpmorgen
; Initial revision
;
; Revision 1.3  2011/09/16 13:50:35  jpmorgen
; Simplified widget hierarchy to try to speed up.  Made insensitive when
; we are tied
;
; Revision 1.2  2011/09/08 20:01:33  jpmorgen
; Cleaned up/created update of widgets at pfo_parinfo_obj level
;
; Revision 1.1  2011/09/01 22:12:12  jpmorgen
; Initial revision
;
;-
function pfo_poly_order_cw_obj::event, event

  ;; We will always swallow the event
  retval = !tok.nowhere

  widget_control, self.ID, get_value=order

  ;; No change means swallow the event
  if self.order eq order then $
     return, retval

  if order lt 0 or order gt 99 then begin
     ok = dialog_message('ERROR: order ' + strtrim(order,2) + ' is out of range', $
                        dialog_parent=self.parentID)
     return, retval
  endif

  ;; If we made it here, we have changed the order to a valid value

  ;; Handle the cases of subtracting and adding coefs separately

  if order lt self.order then begin
     ;; Delete coefs
     ;; Handle undo by hand, since this is a multi-step process
     self.pfo_obj->prepare_update, undo
     self.pfo_obj->parinfo_call_procedure, $
        /no_update, 'pfo_mode', 'delete', idx=(*self.pidx)[order+1:N_elements(*self.pidx)-1]
     self.pfo_obj->parinfo_call_procedure, $
        /no_update, 'pfo_gc'
     self.pfo_obj->update, undo
  endif else begin
     ;; Add coefs
     ;; Extract the current highest order coef
     new_parinfo = self.pfo_obj->parinfo(idx=(*self.pidx)[N_elements(*self.pidx)-1])
     ;; Zero the value
     new_parinfo.value = 0
     ;; Zero the coef ftype
     new_parinfo.pfo.ftype -= self.order * 1E-5
     ;; Replicate by the number of new terms
     new_parinfo = replicate(temporary(new_parinfo), order - self.order)
     ;; Generate an array of of abscissa exponents/coef numbers
     earray = self.order + 1 + indgen(order - self.order)
     ;; Put in new coef ftypes
     new_parinfo.pfo.ftype = new_parinfo.pfo.ftype + earray * 1E-5
     ;; Put in new coef numbers
     new_parinfo.parname = strmid(new_parinfo.parname, 0, strlen(new_parinfo.parname) - 1) + strtrim(earray,2)
     ;; Tack the new coefs onto the end of the existing parinfo
     self.pfo_obj->append_parinfo, parinfo=new_parinfo
  endelse ;; add or subtract coefs

  return, retval
end

pro pfo_poly_order_cw_obj::populate, $
   _REF_EXTRA=extra ;; for now, swallow any extra keywords

  self.ID = pfo_cw_field(self.tlbID, title='Order:', /integer, /return_events, $
                         xsize=3, /kbrd_focus_events, $
                         value=self.order, $
                         uvalue={method:'event', obj:self})
end

;; Cleanup method
pro pfo_poly_order_cw_obj::cleanup
  ;; Call our inherited cleaup routines
  self->pfo_parinfo_cw_obj::cleanup
end

;; Init method
function pfo_poly_order_cw_obj::init, $
   parentID, $ ;; widgetID of parent widget
   order=order, $ ;; current order of polynomial.  Changing this forces repopulate
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

  ;; Call our inherited init routines.  This puts pfo_obj and idx into
  ;; self, among other things.  The idx that is stored in this case is
  ;; the idx of the specific coefs in order.
  ok = self->pfo_parinfo_cw_obj::init(parentID, _EXTRA=extra)
  if NOT ok then return, 0

  ;; No point in registering ourselves in the refresh list, since
  ;; any change in order would result in a repopulate

  ;; Store our local property
  self.order = order

  ;; Build our widget
  self->populate, _EXTRA=extra

  ;; If we made it here, we have successfully set up our widget.
  return, 1

end

;; Object class definition
pro pfo_poly_order_cw_obj__define
  objectClass = $
     {pfo_poly_order_cw_obj, $
      ID	: 0L, $ ;; widget ID of child that does all the work
      order	: 0, $ ;; current order of displayed polynomial
      inherits pfo_parinfo_cw_obj}
end

function pfo_poly_order_cw, $
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

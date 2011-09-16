;+
; NAME: pfo_parinfo_fop_cw
;
; PURPOSE: create a compound widget that displays 'fop', the operation
; that combines the instance of the function described by parinfo[idx]
; with the rest of the functions in parinfo
; (e.g. the '+' in: Y = Y + funct())
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
; $Id: pfo_parinfo_fop_cw.pro,v 1.3 2011/09/16 13:51:03 jpmorgen Exp $
;
; $Log: pfo_parinfo_fop_cw.pro,v $
; Revision 1.3  2011/09/16 13:51:03  jpmorgen
; Simplified widget hierarchy to try to speed up.
;
; Revision 1.2  2011/09/08 20:14:59  jpmorgen
; Cleaned up/created update of widgets at pfo_parinfo_obj level
;
; Revision 1.1  2011/09/01 22:19:48  jpmorgen
; Initial revision
;
;-

function pfo_parinfo_fop_cw_obj::event, event

  sn = tag_names(event, /structure_name)
  if sn ne 'WIDGET_DROPLIST' then $
     message, 'ERROR: unexpected event'

  ;; We will always swallow the event
  retval = !tok.nowhere

  ;; We always list our current choice first and everything else in
  ;; order, so the desired value is the event.index-1.  Of course if
  ;; index = 0, there is no change
  if event.index eq 0 then $
     return, retval

  ;; If we made it here, we have a valid change

  ;; Write it into the parinfo
  self.pfo_obj->parinfo_call_procedure, $
     /save_undo, 'pfo_struct_setget_tag', /set, idx=*self.pidx, $
     taglist_series='pfo', fop=event.index-1

  return, retval
end

;; Helper function
function pfo_parinfo_fop_cw_obj::get_fop_string

  ;; Get output fop and fop current values
  self.pfo_obj->parinfo_call_procedure, $
     /no_update, 'pfo_struct_setget_tag', /get, idx=*self.pidx, $
     taglist_series='pfo', fop=fop, ftype=ftype

  junk = uniq(fop, sort(fop))
  if N_elements(junk) ne 1 then begin
     message, 'ERROR: more than one fop specified for ' + pfo_fname(ftype[0], pfo_obj=self.pfo_obj) + ' idx = ' + pfo_array2str(*self.pidx) + '.  If this function can really work that way, call this widget on each parameter individually'
  endif
  ;; Convert outfop to a string
  return, !pfo.widget_fop_string[fop[0]]

end

;; Refresh method.  pfo_cw_obj handles error catching
pro pfo_parinfo_fop_cw_obj::refresh

  ;; Refresh our display
  widget_control, self.fopID, $
                  set_value=[self->get_fop_string(), !pfo.widget_fop_string]
end

pro pfo_parinfo_fop_cw_obj::populate, $
   _REF_EXTRA=extra ;; for now, swallow any extra keywords

  self.fopID = widget_droplist( $
               self.tlbID, $
               value=[self->get_fop_string(), !pfo.widget_fop_string], $
               uvalue={method: 'event', obj:self})

end

;; Cleanup method
pro pfo_parinfo_fop_cw_obj::cleanup
  ;; Call our inherited cleaup routines
  self->pfo_parinfo_cw_obj::cleanup
end

;; Init method
function pfo_parinfo_fop_cw_obj::init, $
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

  ;; Register ourselves in the refresh list
  self->register_refresh

  ;; Build our widget
  self->populate, _EXTRA=extra

  ;; If we made it here, we have successfully set up our widget.  
  return, 1

end

;; Object class defopion
pro pfo_parinfo_fop_cw_obj__define
  objectClass = $
     {pfo_parinfo_fop_cw_obj, $
      fopID	: 0L, $ ;; widget ID(s) used in refresh method
      inherits pfo_parinfo_cw_obj}
end

function pfo_parinfo_fop_cw, $
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
  ;;cw_obj = obj_new('pfo_parinfo_fop_cw_obj', parentID, _EXTRA=extra)
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

;+
; NAME: pfo_parinfo_xfunct_cw
;
; PURPOSE: create a compound widget that displays (optional)
; transformation ("x") on the input/output axis
; (e.g. the 'alog(' in: Y = Y + funct(alog(Xin)))
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
; $Id: pfo_parinfo_xfunct_cw.pro,v 1.5 2012/01/13 21:00:33 jpmorgen Exp $
;
; $Log: pfo_parinfo_xfunct_cw.pro,v $
; Revision 1.5  2012/01/13 21:00:33  jpmorgen
; Change to widget_combobox so user can enter their own function
;
; Revision 1.4  2011/11/21 15:30:07  jpmorgen
; Add exp10 and alog10
;
; Revision 1.3  2011/09/16 13:49:43  jpmorgen
; Simplified widget hierarchy to try to speed up
;
; Revision 1.2  2011/09/08 20:01:39  jpmorgen
; Cleaned up/created update of widgets at pfo_parinfo_obj level
;
; Revision 1.1  2011/09/01 22:14:38  jpmorgen
; Initial revision
;
;-

function pfo_parinfo_xfunct_cw_obj::event, event

  sn = tag_names(event, /structure_name)
  if sn ne 'WIDGET_COMBOBOX' then $
     message, 'ERROR: unexpected event'

  ;; We will always swallow the event
  retval = !tok.nowhere

  ;; We always list our current choice first and everything else in
  ;; order, so the desired value is the event.index-1.  Of course if
  ;; index = 0, there is no change
  if event.index eq 0 then $
     return, retval

  ;; Check for a user-entered value
  if event.index eq -1 then begin
     xfunct = event.str
  endif else begin
     ;; We have one of our pre-defined functions
     ;; Translate 'no funct' to null string
     xfunct = (*self.pxfuncts)[event.index-1]
     if xfunct eq 'no funct' then $
        xfunct = ''
  endelse ;; index -1 or > 0

  ;; Write it into the parinfo
  if keyword_set(self.in) then $
     self.pfo_obj->parinfo_call_procedure, $
     /save_undo, 'pfo_struct_setget_tag', /set, idx=*self.pidx, $
     taglist_series='pfo', infunct=xfunct

  ;; Change our value(s) in the parinfo.  This catches any errors and
  ;; issues a refresh, if possible instead of a repopulate
  if keyword_set(self.out) then $
     self.pfo_obj->parinfo_call_procedure, $
     /save_undo, 'pfo_struct_setget_tag', /set, idx=*self.pidx, $
     taglist_series='pfo', outfunct=xfunct

  return, retval
end

;; Helper function
function pfo_parinfo_xfunct_cw_obj::get_xfunct

  ;; Get current axis and ftype values
  self.pfo_obj->parinfo_call_procedure, $
     /no_update, 'pfo_struct_setget_tag', /get, idx=*self.pidx, $
     taglist_series='pfo', infunct=infunct, outfunct=outfunct, ftype=ftype

  if self.in then begin
     junk = uniq(infunct, sort(infunct))
     if N_elements(junk) ne 1 then begin
        message, 'ERROR: more than one infunct specified for ' + pfo_fname(ftype[0], pfo_obj=self.pfo_obj) + ' idx = ' + pfo_array2str(*self.pidx) + '.  If this function can really work that way, call this widget on each parameter individually'
     endif
     retval = infunct[0] 
  endif ;; in

  if self.out then begin
     junk = uniq(outfunct, sort(outfunct))
     if N_elements(junk) ne 1 then begin
        message, 'ERROR: more than one outfunct specified for ' + pfo_fname(ftype[0], pfo_obj=self.pfo_obj) + ' idx = ' + pfo_array2string(*self.pidx) + '.  If this function can really work that way, call this widget on each parameter individually'
     endif
     retval = outfunct[0]
  endif ;; out

  ;; Turn null string into 'no funct'
  if retval eq '' then $
     retval = ' no funct'

  return, retval

end

;; Refresh method.  pfo_cw_obj handles error catching
pro pfo_parinfo_xfunct_cw_obj::refresh

  ;; Refresh our display
  widget_control, self.xfunctID, $
                  set_value=[self->get_xfunct(), $
                             *self.pxfuncts]

end

pro pfo_parinfo_xfunct_cw_obj::populate, $
   _REF_EXTRA=extra ;; for now, swallow any extra keywords

  self.xfunctID = widget_combobox(/editable,  $
                  self.tlbID, $
                  value=[self->get_xfunct(), $
                         *self.pxfuncts], $
                  uvalue={method: 'event', obj:self})

end

;; Cleanup method
pro pfo_parinfo_xfunct_cw_obj::cleanup
  ;; Free our heap variables
  ptr_free, self.pxfuncts
  ;; Call our inherited cleaup routines
  self->pfo_parinfo_cw_obj::cleanup
end

;; Init method
function pfo_parinfo_xfunct_cw_obj::init, $
   parentID, $ ;; widgetID of parent widget
   in=in, $ ;; display pfo.infunct
   out=out, $ ;; display pfo.outfunct
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
  if keyword_set(in) + keyword_set(out) ne 1 then $
     message, 'ERROR: use either /in or /out'

  ;; Call our inherited init routines.  This puts pfo_obj into self,
  ;; among other things
  ok = self->pfo_parinfo_cw_obj::init(parentID, _EXTRA=extra)
  if NOT ok then return, 0

  ;; Handle initialization of our allowed axis types in each case
  if keyword_set(in) then begin
     self.pxfuncts = ptr_new(['no funct', 'alog', 'alog10'])
  endif
  if keyword_set(out) then begin
     self.pxfuncts = ptr_new(['no funct', 'exp', 'exp10'])
  endif

  self.in = keyword_set(in)
  self.out = keyword_set(out)

  ;; Register ourselves in the refresh list.
  self->register_refresh

  ;; Build our widget
  self->populate, _EXTRA=extra

  ;; If we made it here, we have successfully set up our widget.
  return, 1

end

;; Object class definition
pro pfo_parinfo_xfunct_cw_obj__define
  objectClass = $
     {pfo_parinfo_xfunct_cw_obj, $
      in	: 0B, $ ;; display pfo.infunct
      out	: 0B, $ ;; display pfo.outfunct
      pxfuncts	: ptr_new(), $ ;; list of allowed axis tokens (e.g. Xin not allowed for outfunct)
      xfunctID	: 0L, $ ;; widget ID(s) used in refresh method
      inherits pfo_parinfo_cw_obj}
end

function pfo_parinfo_xfunct_cw, $
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

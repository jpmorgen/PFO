;+
; NAME: pfo_parinfo_axis_cw
;
; PURPOSE: create a compound widget that displays the input/output axis
; (e.g. the 'Y' in: Y = Y + funct())
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
; $Id: pfo_parinfo_axis_cw.pro,v 1.1 2011/09/01 22:11:28 jpmorgen Exp $
;
; $Log: pfo_parinfo_axis_cw.pro,v $
; Revision 1.1  2011/09/01 22:11:28  jpmorgen
; Initial revision
;
;-

function pfo_parinfo_axis_cw_obj::event, event

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

  ;; Get ready to change our value(s) in the parinfo
  self->repopfresh_check, undo

  ;; Write it into the parinfo
  if keyword_set(self.in) then $
     self.pfo_obj->parinfo_call_procedure, $
     'pfo_struct_setget_tag', /set, idx=*self.pidx, $
     taglist_series='pfo', inaxis=(*self.paxes)[event.index-1]

  ;; Change our value(s) in the parinfo.  This catches any errors and
  ;; issues a refresh, if possible instead of a repopulate
  if keyword_set(self.out) then $
     self.pfo_obj->parinfo_call_procedure, $
     'pfo_struct_setget_tag', /set, idx=*self.pidx, $
     taglist_series='pfo', outaxis=(*self.paxes)[event.index-1]

  ;; Change our value(s) in the parinfo.  This catches any errors and
  ;; issues a refresh, if possible instead of a repopulate
  self->repopfresh_check, undo

  return, retval
end

;; Helper function
function pfo_parinfo_axis_cw_obj::get_axis_string

  ;; Get current axis and ftype values
  self.pfo_obj->parinfo_call_procedure, $
     'pfo_struct_setget_tag', /get, idx=*self.pidx, $
     taglist_series='pfo', inaxis=inaxis, outaxis=outaxis, ftype=ftype

  if self.in then begin
     junk = uniq(inaxis, sort(inaxis))
     if N_elements(junk) ne 1 then begin
        message, 'ERROR: more than one inaxis specified for ' + pfo_fname(ftype[0], pfo_obj=self.pfo_obj) + ' idx = ' + pfo_array2str(*self.pidx) + '.  If this function can really work that way, call this widget on each parameter individually'
     endif
     ;; Convert outaxis to a string
     return, !pfo.widget_axis_string[inaxis[0]]
  endif ;; in

  if self.out then begin
     junk = uniq(outaxis, sort(outaxis))
     if N_elements(junk) ne 1 then begin
        message, 'ERROR: more than one outaxis specified for ' + pfo_fname(ftype[0], pfo_obj=self.pfo_obj) + ' idx = ' + pfo_array2string(*self.pidx) + '.  If this function can really work that way, call this widget on each parameter individually'
     endif
     ;; Convert outaxis to a string
     return, !pfo.widget_axis_string[outaxis[0]]
  endif ;; out

end

;; Refresh method.  pfo_cw_obj handles error catching
pro pfo_parinfo_axis_cw_obj::refresh

  ;; Refresh our display
  widget_control, self.axisID, $
                  set_value=[self->get_axis_string(), $
                             !pfo.widget_axis_string[*self.paxes]]
end

pro pfo_parinfo_axis_cw_obj::populate, $
   _REF_EXTRA=extra ;; for now, swallow any extra keywords

  self.axisID = widget_droplist( $
                self.containerID, $
                value=[self->get_axis_string(), $
                       !pfo.widget_axis_string[*self.paxes]], $
                 uvalue={method: 'event', obj:self})

end

;; Cleanup method
pro pfo_parinfo_axis_cw_obj::cleanup
  ;; Free our heap variables
  ptr_free, self.paxes
  ;; Call our inherited cleaup routines
  self->pfo_parinfo_cw_obj::cleanup
end

;; Init method
function pfo_parinfo_axis_cw_obj::init, $
   parentID, $ ;; widgetID of parent widget
   in=in, $ ;; display pfo.inaxis
   out=out, $ ;; display pfo.outaxis
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
     self.paxes = ptr_new([!pfo.none, !pfo.Xin, !pfo.Xaxis, !pfo.Yaxis])
  endif
  if keyword_set(out) then begin
     self.paxes = ptr_new([!pfo.none, !pfo.Xaxis, !pfo.Yaxis])
  endif

  self.in = keyword_set(in)
  self.out = keyword_set(out)

  ;; Register ourselves in the refresh list.  Since we have a list of
  ;; idx, just use the first one
  self->register_refresh


  ;; Create our container widget
  self->create_container

  ;; Build our widget
  self->populate, _EXTRA=extra

  ;; If we made it here, we have successfully set up our container.  
  return, 1

end

;; Object class definition
pro pfo_parinfo_axis_cw_obj__define
  objectClass = $
     {pfo_parinfo_axis_cw_obj, $
      in	: 0B, $ ;; display pfo.inaxis
      out	: 0B, $ ;; display pfo.outaxis
      paxes	: ptr_new(), $ ;; list of allowed axis tokens (e.g. Xin not allowed for outaxis)
      axisID	: 0L, $ ;; widget ID(s) used in refresh method
      inherits pfo_parinfo_cw_obj}
end

function pfo_parinfo_axis_cw, $
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

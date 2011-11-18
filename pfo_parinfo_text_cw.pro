;+
; NAME: pfo_parinfo_text_cw
;
; PURPOSE: display a (segment of) parinfo in a text widget
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
; $Id: pfo_parinfo_text_cw.pro,v 1.3 2011/11/18 15:34:13 jpmorgen Exp $
;
; $Log: pfo_parinfo_text_cw.pro,v $
; Revision 1.3  2011/11/18 15:34:13  jpmorgen
; Changed to use pfo_obj print method
;
; Revision 1.2  2011/09/23 13:05:29  jpmorgen
; Fixed bug
;
; Revision 1.1  2011/09/16 11:30:15  jpmorgen
; Initial revision
;
;-

;; Helper procedure
pro pfo_parinfo_text_cw_obj::get_text, $
   text=text, $ ;; output of pfo_parinfo_parse
   nlines=nlines, $ ;; number of lines for ysize
   width=width, $ ;; maximum number of characters in any one line
   status=status, $ ;; see if we should modify our appearance
   _REF_EXTRA=extra ;; extra args passed to pfo_parinfo_parse

  ;; Use pfo_parinfo_parse to get our parinfo (segment)
  ;; --> might need to have a way of capturing _EXTRA as property to
  ;; modify printing style
  text = self.pfo_obj->print(/full, $
         /no_preamble, idx=*self.pidx, status_mask=!pfo.all_status, _EXTRA=extra)

  ;; Get our status to see if we should modify our appearance
  self.pfo_obj->parinfo_call_procedure, $
     /no_update, 'pfo_struct_setget_tag', /get, idx=*self.pidx, $
     taglist_series='pfo', status=status

  ;; Make sure we have a unique status value in our parinfo
  if N_elements(uniq(status, sort(status))) ne 1 then $
     message, 'ERROR: non-unique pfo.status tag for this function.'
  status = status[0]

  ;; Convert text to an array so Windows widget_text newlines work right
  text = pfo_string2array(text, maxwidth=width)
  nlines = N_elements(text)

end

;; Refresh method.  pfo_cw_obj handles error catching
pro pfo_parinfo_text_cw_obj::refresh, $
   _EXTRA=extra

  ;; Refresh our display
  self->get_text, text=text, nlines=nlines, width=width, status=status
  
  ;; Put it in the widget
  widget_control, self.ID, set_value=text, xsize=width, ysize=nlines, $
                  sensitive=(status eq !pfo.active)

end

pro pfo_parinfo_text_cw_obj::populate, $
   _REF_EXTRA=extra ;; extra args passed to pfo_parinfo_parse

  self->get_text, text=text, nlines=nlines, width=width, status=status, _EXTRA=extra

  ;; Select a fixed-width font we know is going to be there so things
  ;; line up with FORMAT statements. --> a better font could be found
  ;; -- what do they use for the terminal window in IDL?
  if !d.name eq 'WIN' then $
     font = !pfo.win_font

  self.ID = widget_text(self.tlbID, value=text, xsize=width, $
                        ysize=nlines, font=font, editable=0, $
                        sensitive=(status eq !pfo.active))

end

;; Cleanup method
pro pfo_parinfo_text_cw_obj::cleanup
  ;; Call our inherited cleaup routines
  self->pfo_parinfo_cw_obj::cleanup
end

;; Init method
function pfo_parinfo_text_cw_obj::init, $
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
  ;; among other things.  Turn off tabbing into this widget --> not
  ;; working at the moment.
  ok = self->pfo_parinfo_cw_obj::init(parentID, tab_mode=0, _EXTRA=extra)
  if NOT ok then return, 0

  ;; Register ourselves in the refresh list
  self->register_refresh

  ;; Build our widget
  self->populate, _EXTRA=extra

  ;; If we made it here, we have successfully set up our widget.
  return, 1

end

;; Object class definition
pro pfo_parinfo_text_cw_obj__define
  objectClass = $
     {pfo_parinfo_text_cw_obj, $
      ID	: 0L, $ ;; widget ID of child that does all the work
      inherits pfo_parinfo_cw_obj}
end

function pfo_parinfo_text_cw, $
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

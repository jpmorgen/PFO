;+
; NAME: pfo_cursor_cw
;
; PURPOSE: displays current cursor position in a pfo_plotwin
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
; $Id: pfo_cursor_cw.pro,v 1.3 2012/03/23 01:50:24 jpmorgen Exp $
;
; $Log: pfo_cursor_cw.pro,v $
; Revision 1.3  2012/03/23 01:50:24  jpmorgen
; Change event registration syntax
;
; Revision 1.2  2011/11/18 14:47:58  jpmorgen
; Made column headings a separate widet
;
; Revision 1.1  2011/11/11 15:48:44  jpmorgen
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
function pfo_cursor_cw_obj::plotwin_event, event

  ;; We will always swallow the event
  retval = !tok.nowhere

  ;; Check to make sure we were forwarded the proper event.
  sn = tag_names(event, /structure_name)
  if sn ne 'WIDGET_DRAW' then $
     message, 'ERROR: expecting a WIDGET_DRAW event, got ' + sn

  ;; Check to make sure our plumbing was connected properly
  if NOT obj_valid(self.plotwin_obj) then $ 
     message, 'ERROR: plotwin_obj not properly registered in this object'

  ;; Convert from device coords (what event.[xy] are reported in) to data
  coords = self.plotwin_obj->get_pfo_coord(event.x, event.y, /device)
  for iw=0,2 do begin
     widget_control, self.IDs[iw], set_value=coords[iw]
  endfor ;; each widget

  return, retval

end

;; Register our event with the the plot_obj event fowarding system.
;; We just care about mouse motion events, which is non-disruptive to
;; the event flow, so we can be a persistent event handler
pro pfo_cursor_cw_obj::register_forward

  if NOT obj_valid(self.plotwin_obj) then $
     message, 'ERROR: plotwin_obj is not yet registered with this obj.  Use set_property, plotwin_obj=plotwin_obj or pass plotwin_obj when initializing this object'

  self.plotwin_obj->register_forward, /persistent, $
     {method:'plotwin_event', obj:self}, /draw_motion_events
end

pro pfo_cursor_cw_obj::populate, $
   _REF_EXTRA=extra ;; for now, swallow any extra keywords
  
  ;; Make the cursor widgets.
  rowID = widget_base(self.tlbID, /row)
  ;; Label is the "min/max" column
  ID = widget_label(rowID, value='', xsize=self.label_width, units=self.units)
  for iw=0,2 do begin
     ID = widget_base(rowID, xsize=self.col_width, units=self.units)
     self.IDs[iw] = pfo_cw_field(ID, title='', /float, $
                                 value=!values.d_NAN, /noedit)
  endfor ;; each widget

end

;; Cleanup method
pro pfo_cursor_cw_obj::cleanup
  ;; Call our inherited cleaup routines
  self->pfo_cw_obj::cleanup
  self->pfo_plotwin_cw_obj::cleanup
end

;; Init method
function pfo_cursor_cw_obj::init, $
   parentID, $ ;; widgetID of parent widget
   label_width=label_width, $ ;; width of column to the left of Xin, Xaxis, Yaxis columns (e.g. for min/max label)
   col_width=col_width, $ ;; width of Xin, Xaxis, and Yaxis columns
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
  
  ;; Set up our local property (a set/get_property routine might be
  ;; useful for these)
  self.units = !tok.inches
  self.label_width = 0.5
  if N_elements(label_width) ne 0 then self.label_width = label_width
  self.col_width = 1.25
  if N_elements(col_width) ne 0 then self.col_width = col_width

  ;; Call our inherited init routines.
  ok = self->pfo_plotwin_cw_obj::init(_EXTRA=extra)
  if NOT ok then return, 0
  ;; Make a column base to accept the rows of widgets we will create here
  ok = self->pfo_cw_obj::init(parentID, /column, _EXTRA=extra)
  if NOT ok then return, 0

  ;; Build our widget
  self->populate, _EXTRA=extra

  ;; Register our event handler if we know our plotwin at this point.
  if obj_valid(self.plotwin_obj) then $
     self->register_forward

  ;; If we made it here, we have successfully set up our widget.
  return, 1

end

;; Object class definition
pro pfo_cursor_cw_obj__define
  objectClass = $
     {pfo_cursor_cw_obj, $
      label_width	: 0., $
      col_width		: 0., $
      units		: 0, $
      IDs		: lonarr(3), $ ;; widget IDs of pfo_cw_fields which display cursor values
      inherits pfo_plotwin_cw_obj, $
      inherits pfo_cw_obj}
end

function pfo_cursor_cw, $
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

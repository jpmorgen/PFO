;+
; NAME: pfo_cursor_colhead_cw
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
; $Id: pfo_cursor_colhead_cw.pro,v 1.2 2013/05/24 22:43:00 jpmorgen Exp $
;
; $Log: pfo_cursor_colhead_cw.pro,v $
; Revision 1.2  2013/05/24 22:43:00  jpmorgen
; Tweak things a bit.  Give to Ron
;
; Revision 1.1  2011/11/18 14:40:59  jpmorgen
; Initial revision
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
;; Get the strings that correspond to the column headings of our widgets
pro pfo_cursor_colhead_cw_obj::col_heads, $
   Xin_colhead=Xin_colhead, $
   Xaxis_colhead=Xaxis_colhead, $
   Yaxis_colhead=Yaxis_colhead

  ;; Set up a default plot_obj in case our plotwin hasn't received any
  ;; input yet.  This uses the generic plot_obj stored in our pfo_obj,
  ;; which is usually what we use anyway
  plot_obj = self.pfo_obj
  ;; Check to make sure we are connected to a plotwin yet.  This is
  ;; something that can occur after the fact....
  if obj_valid(self.plotwin_obj) then $
     if obj_valid(self.plotwin_obj->last_pfo_plot_obj()) then $
        plot_obj = self.plotwin_obj->last_pfo_plot_obj()

  ;; Extract title information from the plot_obj
  plot_obj->get_property, $
     plot_Xunits=Xunits, $
     plot_Yunits=Yunits, $
     plot_Xin_title=Xin_title, $
     plot_Xin_units=Xin_units, $
     plot_Xaxis_title=Xaxis_title, $
     plot_Xaxis_units=Xaxis_units, $
     plot_Yin_Xin_title=Yin_Xin_title, $
     plot_Yin_Xin_units=Yin_Xin_units, $
     plot_Yin_Xaxis_title=Yin_Xaxis_title, $
     plot_Yin_Xaxis_units=Yin_Xaxis_units

  ;; Prefer the units, since the are likely to be shorter
  Xin_colhead = Xin_units
  if Xin_colhead eq '' then $
     Xin_colhead = Xin_title
  Xaxis_colhead = Xaxis_units
  if Xaxis_colhead eq '' then $
     Xaxis_colhead = Xaxis_title

  ;; Use the modes to set our xtitle, xaxis, etc.
  case Xunits of 
     !pfo.Xin: begin
        Yaxis_colhead = Yin_Xin_units
        if Yaxis_colhead eq '' then $
           Yaxis_colhead = Yin_Xin_title
     end
     !pfo.Xaxis: begin
        case Yunits of
           !pfo.Xin: begin
              Yaxis_colhead = Yin_Xin_units
              if Yaxis_colhead eq '' then $
                 Yaxis_colhead = Yin_Xin_title
           end
           !pfo.Xaxis: begin
              Yaxis_colhead = Yin_Xaxis_units
              if Yaxis_colhead eq '' then $
                 Yaxis_colhead = Yin_Xaxis_title
           end
           else: message, 'ERROR: invalid Yunits value: ' + strtrim(Xunits, 2) + ' expecting !pfo.Xin or !pfo.Xaxis'
        endcase
     end
     else: message, 'ERROR: invalid Xunits value: ' + strtrim(Xunits, 2) + ' expecting !pfo.Xin or !pfo.Xaxis'
  endcase

end

;; Refresh event, just in case something changes
pro pfo_cursor_colhead_cw_obj::refresh

  self->col_heads, $
   Xin_colhead=Xin_colhead, $
   Xaxis_colhead=Xaxis_colhead, $
   Yaxis_colhead=Yaxis_colhead

  widget_control, self.IDs[0], set_value=Xin_colhead
  widget_control, self.IDs[1], set_value=Xaxis_colhead
  widget_control, self.IDs[2], set_value=Yaxis_colhead

end

pro pfo_cursor_colhead_cw_obj::populate, $
   _REF_EXTRA=extra ;; for now, swallow any extra keywords
  
  self->col_heads, $
   Xin_colhead=Xin_colhead, $
   Xaxis_colhead=Xaxis_colhead, $
   Yaxis_colhead=Yaxis_colhead

  ;; Make the column heading widgets
  rowID = widget_base(self.tlbID, /row)
  ID = widget_label(rowID, value='', xsize=self.label_width, units=self.units)
  self.IDs[0] = widget_label(rowID, value=Xin_colhead, xsize=self.col_width, units=self.units)
  self.IDs[1] = widget_label(rowID, value=Xaxis_colhead, xsize=self.col_width, units=self.units)
  self.IDs[2] = widget_label(rowID, value=Yaxis_colhead, xsize=self.col_width, units=self.units)

end

;; Cleanup method
pro pfo_cursor_colhead_cw_obj::cleanup
  ;; Call our inherited cleaup routines
  self->pfo_cw_obj::cleanup
end

;; Init method
function pfo_cursor_colhead_cw_obj::init, $
   parentID, $ ;; widgetID of parent widget
   label_width=label_width, $ ;; width of column to the left of Xin, Xaxis, Yaxis columns (e.g. for min/max label)
   col_width=col_width, $ ;; width of columns containing Xin, Xaxis, Yaxis
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
  self.label_width = 0.
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

  ;; Register ourself in the refresh list
  self.pfo_obj->register_refresh, self

  ;; If we made it here, we have successfully set up our widget.
  return, 1

end

;; Object class definition
pro pfo_cursor_colhead_cw_obj__define
  objectClass = $
     {pfo_cursor_colhead_cw_obj, $
      label_width	: 0., $
      col_width		: 0., $
      units		: '', $
      IDs		: lonarr(3), $ ;; widget IDs of pfo_cw_fields which display cursor values
      inherits pfo_plotwin_cw_obj, $
      inherits pfo_cw_obj}
end

function pfo_cursor_colhead_cw, $
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

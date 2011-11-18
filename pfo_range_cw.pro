;+
; NAME: pfo_range_cw
;
; PURPOSE: Allows user to adjust plot axis ranges by hand
;
; CATEGORY: PFO widgets
;
; CALLING SEQUENCE:
;
; DESCRIPTION:  NOTE: this widget must connect to both a pfo_obj and a
; plotwin_obj.  The plotwin_obj tells this widegt exactly what is
; being displayed in the window
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
; $Id: pfo_range_cw.pro,v 1.1 2011/11/18 14:40:49 jpmorgen Exp $
;
; $Log: pfo_range_cw.pro,v $
; Revision 1.1  2011/11/18 14:40:49  jpmorgen
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
function pfo_range_cw_obj::event, event, location=location

  ;; We will always swallow the event
  retval = !tok.nowhere

  ;; Set up a default plot_obj in case our plotwin hasn't received any
  ;; input yet.  This uses the generic plot_obj stored in our pfo_obj,
  ;; which is usually what we use anyway
  plot_obj = self.pfo_obj
  ;; Check to make sure we are connected to a plotwin yet.  This is
  ;; something that can occur after the fact....
  if obj_valid(self.plotwin_obj) then $
     if obj_valid(self.plotwin_obj->last_pfo_plot_obj()) then $
        plot_obj = self.plotwin_obj->last_pfo_plot_obj()

  ;; Extract existing ranges
  plot_obj->get_property, $
     plot_Xin_range=plot_Xin_range, $
     plot_Xaxis_range=plot_Xaxis_range, $
     plot_Yaxis_range=plot_Yaxis_range

  ;; Check to see if we have changed our value.  If so, modify the
  ;; plot_obj property.  Otherwise, return, swallowing the event
  widget_control, event.ID, get_value=w_value
  case location[1]+1 of 
     !pfo.Xin: begin
        if plot_Xin_range[location[0]] eq w_value then $
           return, retval
        plot_Xin_range[location[0]] = w_value
        plot_obj->set_property, $
           plot_Xin_range=plot_Xin_range
     end
     !pfo.Xaxis: begin
        if plot_Xaxis_range[location[0]] eq w_value then $
           return, retval
        plot_Xaxis_range[location[0]] = w_value
        plot_obj->set_property, $
           plot_Xaxis_range=plot_Xaxis_range
     end
     !pfo.Yaxis: begin
        if plot_Yaxis_range[location[0]] eq w_value then $
           return, retval
        plot_Yaxis_range[location[0]] = w_value
        plot_obj->set_property, $
           plot_Yaxis_range=plot_Yaxis_range
     end
     else: message, 'ERROR: invalid location[1] (axis) value ' + strtrim(location[1], 2)
  endcase

  return, retval

end

;; Refresh method.  pfo_cw_obj handles error catching
pro pfo_range_cw_obj::refresh

  ;; Set up a default array of ranges.  Min,Max x Xin,Xaxis,Yaxis
  ra = make_array(2, 3, value=!values.d_NAN)

  ;; See if we have a valid last_pfo_plot_obj
  if obj_valid(self.plotwin_obj) then begin
     if obj_valid(self.plotwin_obj->last_pfo_plot_obj()) then begin
        (self.plotwin_obj->last_pfo_plot_obj())->get_property, $
           plot_Xin_range    = plot_Xin_range      , $
           plot_Xaxis_range  = plot_Xaxis_range    , $
           plot_Yaxis_range  = plot_Yaxis_range
        ;; Put values into our range array
        ra[0, *] = [plot_Xin_range[0], plot_Xaxis_range[0], plot_Yaxis_range[0]]
        ra[1, *] = [plot_Xin_range[1], plot_Xaxis_range[1], plot_Yaxis_range[1]]
     endif ;; valid last_pfo_plot_obj
  endif ;; valid plotwin_obj

  ;; Write our range array into the widgets
  for ir=0,1 do begin
     for ia=0,2 do begin
        widget_control, self.IDs[ir, ia], set_value=ra[ir, ia]
     endfor ;; each axis
  endfor ;; each range (min/max)
end

pro pfo_range_cw_obj::populate, $
   no_colheads=no_colheads, $ ;; don't include column headings
   _REF_EXTRA=extra ;; for now, swallow any extra keywords

  ;; Default is to put in column headings
  if NOT keyword_set(no_colheads) then $
     ID = pfo_cursor_colhead_cw(self.tlbID, label_width=self.label_width, $
                                col_width=self.col_width, pfo_obj=self.pfo_obj, plotwin_obj=self.plotwin_obj)

  ;; Min
  rowID = widget_base(self.tlbID, /row)
  ID = widget_label(rowID, value='min', xsize=self.label_width, units=self.units)
  for iw=0,2 do begin
     ;; Use a base to limit extent of pfo_cw_field widget
     ID = widget_base(rowID, xsize=self.col_width, units=self.units)
     self.IDs[0, iw] = pfo_cw_field(ID, title='', /float, /return_events, $
                                    /kbrd_focus_events, $
                                    value=!values.d_NAN, $
                                    uvalue={method:'event', obj:self, $
                                            keywords:{location:[0,iw]}})
  endfor ;; each widget


  ;; Max
  rowID = widget_base(self.tlbID, /row)
  ID = widget_label(rowID, value='max', xsize=self.label_width, units=self.units)
  for iw=0,2 do begin
     ;; Use a base to limit extent of pfo_cw_field widget
     ID = widget_base(rowID, xsize=self.col_width, units=self.units)
     self.IDs[1, iw] = pfo_cw_field(ID, title='', /float, /return_events, $
                                    /kbrd_focus_events, $
                                    value=!values.d_NAN, $
                                    uvalue={method:'event', obj:self, $
                                            keywords:{location:[1,iw]}})
  endfor ;; each widget

  ;; Use the refresh method to populate the widgets
  self->refresh

end

;; Cleanup method
pro pfo_range_cw_obj::cleanup
  ;; Call our inherited cleaup routines
  self->pfo_cw_obj::cleanup
  self->pfo_plotwin_cw_obj::cleanup
end

;; Init method
function pfo_range_cw_obj::init, $
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
  self.col_width = 1.
  if N_elements(col_width) ne 0 then self.col_width = col_width

  ;; Call our inherited init routines.
  ok = self->pfo_plotwin_cw_obj::init(_EXTRA=extra)
  if NOT ok then return, 0
  ;; Make a column base to accept the rows of widgets we will create here
  ok = self->pfo_cw_obj::init(parentID, /column, _EXTRA=extra)
  if NOT ok then return, 0

  ;; Build our widget
  self->populate, _EXTRA=extra

  ;; Register ourselves in the refresh list in case people change the
  ;; ranges by other means.
  self.pfo_obj->register_refresh, self

  ;; If we made it here, we have successfully set up our widget.
  return, 1

end

;; Object class definition
pro pfo_range_cw_obj__define
  objectClass = $
     {pfo_range_cw_obj, $
      label_width	: 0., $ ;; width of column holding min/max
      col_width		: 0., $ ;; width of Xin, Xaxis, and Yaxis columns
      units		: '', $
      IDs		: lonarr(2,3), $ ;; widget IDs of pfo_cw_fields which range values
      inherits pfo_plotwin_cw_obj, $
      inherits pfo_cw_obj}
end

function pfo_range_cw, $
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

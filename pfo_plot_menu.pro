;+
; NAME: pfo_plot_menu
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
; $Id: pfo_plot_menu.pro,v 1.2 2012/01/13 20:56:16 jpmorgen Exp $
;
; $Log: pfo_plot_menu.pro,v $
; Revision 1.2  2012/01/13 20:56:16  jpmorgen
; Got working
;
; Revision 1.1  2011/11/30 21:03:10  jpmorgen
; Initial revision
;
; Revision 1.1  2011/09/22 23:45:59  jpmorgen
; Initial revision
;
;-

function pfo_plot_menu_obj::event, event, button=button
  
  sn = tag_names(event, /structure_name)
  if sn ne 'WIDGET_BUTTON' then $
     message, 'ERROR: unexpected event type ' + sn

  if N_elements(button) eq 0 then $
     message, 'ERROR: button keyword required'

  ;; Prepare to swallow our event
  retval = !tok.nowhere

  if NOT obj_valid(self.plotwin_obj) then $
     return, retval

  plot_obj = self.plotwin_obj->last_pfo_plot_obj()
  case button of 
     self.xlog: begin
        plot_obj->get_property, plot_xlog=plot_xlog
        plot_obj->set_property, plot_xlog=~plot_xlog
     end
     self.ylog: begin
        plot_obj->get_property, plot_ylog=plot_ylog
        plot_obj->set_property, plot_ylog=~plot_ylog
     end
     self.xin_autoscale: plot_obj->set_property, /plot_Xin_autoscale
     self.xaxis_autoscale: plot_obj->set_property, /plot_Xaxis_autoscale
     self.yaxis_autoscale: plot_obj->set_property, /plot_Yaxis_autoscale
     else : message, 'ERROR: unrecognized button token: ' + button
  endcase

  ;; Swallow event
  return, retval

end

pro pfo_plot_menu_obj::populate, $
   _REF_EXTRA=extra ;; for now, swallow any extra keywords

  ID = widget_button(self.tlbID, value='X log', $
                        uvalue={method: 'event', obj:self, keywords:{button:self.xlog}})
  ID = widget_button(self.tlbID, value='Y log', $
                        uvalue={method: 'event', obj:self, keywords:{button:self.ylog}})
  ID = widget_button(self.tlbID, value='Xin autscale', $
                        uvalue={method: 'event', obj:self, keywords:{button:self.xin_autoscale}})
  ID = widget_button(self.tlbID, value='Xaxis autscale', $
                        uvalue={method: 'event', obj:self, keywords:{button:self.xaxis_autoscale}})
  ID = widget_button(self.tlbID, value='Yaxis autscale', $
                        uvalue={method: 'event', obj:self, keywords:{button:self.yaxis_autoscale}})

end

;; Cleanup method
pro pfo_plot_menu_obj::cleanup
  ;; Call our inherited cleaup routines
  self->pfo_cw_obj::cleanup
  ;; Nothing local to do
end

;; Init method
function pfo_plot_menu_obj::init, $
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

  ;; Initialize our tokens
  self.xlog = 0
  self.ylog = 1

  ;; Call our inherited init routines.
  ok = self->pfo_cw_obj::init(parentID, /menu, value='Plot', _EXTRA=extra)
  if NOT ok then return, 0

  ;; Build our widget
  self->populate, _EXTRA=extra

  ;; This registers our plotwin_obj
  ok = self->pfo_plotwin_cw_obj::init(_EXTRA=extra)
  if NOT ok then return, 0

  ;; If we made it here, we have successfully set up our container.  
  return, 1

end

;; Object class definition.  This widget operates on general pfo_obj
;; property, NOT the parinfo, so it can be a plain pfo_cw_obj
pro pfo_plot_menu_obj__define
  objectClass = $
     {pfo_plot_menu_obj, $
      xlog	: 0B, $ ;; tokens for events
      ylog	: 0B, $
      xin_autoscale	: 0B, $
      xaxis_autoscale	: 0B, $
      yaxis_autoscale	: 0B, $
      inherits pfo_cw_obj, $
      inherits pfo_plotwin_cw_obj}
end

function pfo_plot_menu, $
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

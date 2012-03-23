;+
; NAME: pfo_plotwin_cw_obj__define
;
; PURPOSE: Define common property and methods for pfo_cw_objs that
; connect to pfo_plotwins
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
; $Id: pfo_plotwin_cw_obj__define.pro,v 1.4 2012/03/23 01:47:34 jpmorgen Exp $
;
; $Log: pfo_plotwin_cw_obj__define.pro,v $
; Revision 1.4  2012/03/23 01:47:34  jpmorgen
; Change documentation about event handers
;
; Revision 1.3  2012/01/26 16:24:14  jpmorgen
; Fix mistaken link to pfo_cw_obj.  We are truely independent of that system
;
; Revision 1.2  2011/11/18 15:54:38  jpmorgen
; Minor comment update
;
; Revision 1.1  2011/11/11 15:49:12  jpmorgen
; Initial revision
;
;-

;; Get property method
pro pfo_plotwin_cw_obj::get_property, $
   plotwin_obj=plotwin_obj

  if (arg_present(plotwin_obj) or N_elements(plotwin_obj) ne 0) and $
     obj_valid(self.plotwin_obj) then $
        plotwin_obj = self.plotwin_obj

end

;; Set property method
pro pfo_plotwin_cw_obj::set_property, $
   plotwin_obj=plotwin_obj ;; plotwin_obj to which we will be connected.  Can be left blank at invocation, but needs to be set at some point before commands are issued


  if obj_valid(plotwin_obj) then begin
     ;; Disconnect from any existing plotwin
     if obj_valid(self.plotwin_obj) then $
        self.plotwin_obj->unregister_forward, self
     ;; Store plotwin_obj in our property
     self.plotwin_obj = plotwin_obj
     ;; NOTE: This does not disconnect objects that are in
     ;; pfo_event_forward_objs (like zoom, ROI, and peak mouse
     ;; objects).  But those are unlikely to want to be switched,
     ;; since those tend to stick with the plotwin they are
     ;; ministering to.

  endif ;; plotwin_obj

end

;; Cleanup method
pro pfo_plotwin_cw_obj::cleanup

  ;; Take all event handlers from this obj off of the event forwarding
  ;; list.  This does not cause problems if we weren't registered in
  ;; the first place.  But it does cause problems if we haven't
  ;; been connected or that widget has been killed first
  if obj_valid(self.plotwin_obj) then $
     self.plotwin_obj->unregister_forward, self

end

function pfo_plotwin_cw_obj::init, $
   _REF_EXTRA=extra 	;; all args, such as plotwin_obj, are passed on to ::init

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

  ;; Use our local set_property method to store property and register
  ;; our event handler(s).  Use local method in case we are inherited
  self->pfo_plotwin_cw_obj::set_property, _EXTRA=extra
  
  ;; If we made it here, we have successfully set up our object, it is
  ;; up to our inheriting routines to do the rest
  return, 1

end

pro pfo_plotwin_cw_obj__define
  objectClass = $
     {pfo_plotwin_cw_obj, $
      plotwin_obj	: obj_new() $ ;; plotwin we are connected to
     }
end


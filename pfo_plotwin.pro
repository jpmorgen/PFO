;+
; NAME: pfo_plotwin

; PURPOSE: Creates generic PFO plot window

; CATEGORY: PFO widgets
;
; CALLING SEQUENCE:

; DESCRIPTION: This routine creates a compound widget, administered by
; a pfo_plotwin_obj, which contains a draw_widget.  This draw widget
; is configured to accept IDL direct graphics plots and can be
; configured to forward events to a list of object-oriented event
; handlers.

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
; $Id: pfo_plotwin.pro,v 1.2 2012/03/23 01:50:51 jpmorgen Exp $
;
; $Log: pfo_plotwin.pro,v $
; Revision 1.2  2012/03/23 01:50:51  jpmorgen
; Minor documentation change
;
; Revision 1.1  2011/11/18 15:56:17  jpmorgen
; Initial revision
;
; Revision 1.3  2011/09/22 23:48:07  jpmorgen
; Minor changes
;
; Revision 1.2  2011/09/16 13:49:24  jpmorgen
; Simplified widget hierarchy to try to speed up
;
; Revision 1.1  2011/09/01 22:12:30  jpmorgen
; Initial revision
;
;-

function pfo_plotwin, $
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

  ;; Create our controlling object.  In this case a pfo_plotwin_obj.
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

;+
; NAME: pfo_tROI_obj__define

; PURPOSE: Creates an object which allows the mouse to zoom/unzoom the
; plot in the plotwin to which this object is connected

; CATEGORY: PFO widgets
;
; CALLING SEQUENCE:
;
; DESCRIPTION: This produces an object
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
; $Id: pfo_troi_obj__define.pro,v 1.1 2012/01/26 16:21:48 jpmorgen Exp $
;
; $Log: pfo_troi_obj__define.pro,v $
; Revision 1.1  2012/01/26 16:21:48  jpmorgen
; Initial revision
;
;-

function pfo_tROI_obj::tROI_plotwin_event, event
  ;; We will always swallow the event
  retval = !tok.nowhere

  ;; Check to make sure we were forwarded the proper event.
  sn = tag_names(event, /structure_name)
  if sn ne 'WIDGET_DRAW' then $
     message, 'ERROR: expecting a WIDGET_DRAW event, got ' + sn

  ;; Check to make sure our plumbing was connected properly
  if NOT obj_valid(self.plotwin_obj) then $ 
     message, 'ERROR: plotwin_obj not properly registered in this object'

  ;; Check to make sure we have a plot
  plot_obj = self.plotwin_obj->last_pfo_plot_obj()
  if NOT obj_valid(plot_obj) then $
     return, retval

  ;; If we made it here, we have properly plotted something in our plotwin.

  ;; Convert from device coords (what event.[xy] are reported in) to data
  coords = self.plotwin_obj->get_pfo_coord(event.x, event.y, /device)

  ;; Get the current tROI from the plot_obj
  plot_obj->get_property, tROI=tROI

  ;; Keep track of whether or not we are holding down a mouse button.
  ;; Code borrowed from cw_defroi.  Bitmask 1,2, or 4 (left, middle,
  ;; right) when a button is _pressed_, otherwise 0.
  self.button = (self.button or event.press xor event.release)

  ;; The following three if statements are mutually exclusive.  They
  ;; adjust the local variable tROI, which read in units of Xin, based
  ;; on the mouse/button status.
  
  ;; Check to see if we are starting to define our zoom region
  if event.press eq !tok.left then begin
     ;; By default, we assume the region is going to grow to the right
     self.direction = !tok.right
     ;; Start it off with a null region at the location of the event
     plot_obj->set_property, tROI=replicate(coords[0], 2)
     return, retval
  endif

  ;; Check to see if we are holding down the left mouse button and
  ;; expand our tROI accordingly
  if self.button eq !tok.left then begin
     ;; This logic makes a zoom region that only grows
     ;;tROI[0] = tROI[0] < coords[0]
     ;;tROI[1] = tROI[1] > coords[0]
     ;; This logic fixes one end of the region at the first click.
     ;; The second end follows the mouse.  We need to execute the if
     ;; statements in a different order depending on which direction
     ;; we were last growing
     if self.direction eq !tok.right then begin
        ;; Growing to the right
        if coords[0] gt tROI[0] then begin
           tROI[1] =  coords[0]
           self.direction = !tok.right
        endif
        ;; This only kicks in when we cross over our initial point
        if coords[0] lt tROI[1] then begin
           tROI[0] =  coords[0]
           self.direction = !tok.left
        endif
     endif else begin ;; growing to the right
        ;; Growing to the left
        if coords[0] lt tROI[1] then begin
           tROI[0] =  coords[0]
           self.direction = !tok.left
        endif
        ;; This only kicks in when we cross over our initial point
        if coords[0] gt tROI[1] then begin
           tROI[1] =  coords[0]
           self.direction = !tok.right
        endif
     endelse ;; growing to the left

     ;; Update tROI in the plot_obj
     plot_obj->set_property, tROI=tROI
     return, retval

  endif ;; Holding down left button

  ;; Check to see if we have finished defining a zoom region of any
  ;; substantial length
  if (event.release eq !tok.left) and $
     tROI[1] - tROI[0] gt 0 then begin

     ;; Erase our tROI in the plot_obj, since we are done with it.  Do
     ;; this before we invoke our callback routine, since otherwise,
     ;; we may get undesired blinking
     plot_obj->set_property, tROI=[0d,0d]

     ;; Catch errors in callback procedure
     ;; Handle pfo_debug level.  CATCH errors if _not_ debugging
     if !pfo.debug le 0 then begin
        CATCH, err
        if err ne 0 then begin
           CATCH, /CANCEL
           message, /NONAME, !error_state.msg, /CONTINUE
           message, 'ERROR: caught the above error.  Have you set up your callback procedure properly to capture tROI? ', /CONTINUE
           return, retval
        endif
     endif ;; not debugging

     ;; Use a callback procedure to pass the value back to the
     ;; invoking object
     call_method, self.tROI_set_proc, self.tROI_calling_obj, tROI

     return, retval

  endif ;; releasing left button

  ;; If we made it here, we are not the kind of event we care about.
  ;; Just swallow the event
  return, retval

end

;; Cleanup method
pro pfo_tROI_obj::cleanup
  ;; Call our inherited cleaup routines
  self->pfo_plotwin_cw_obj::cleanup
end

function pfo_tROI_obj::init, $
   tROI_set_proc=tROI_set_proc, $         ;; callback method name used to pass tROI back to
   tROI_calling_obj=tROI_calling_obj, $   ;; calling obj (usually self)
   _REF_EXTRA=extra ;; All input parameters, including pfo_obj, 
  ;; are passed to the init method and underlying routines via
  ;; _REF_EXTRA mechanism

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

  ;; Call our inherited init routines.  This registers our plotwin_obj
  ok = self->pfo_plotwin_cw_obj::init(_EXTRA=extra)
  if NOT ok then return, 0

  ;; Assign our local property
  self.tROI_set_proc = tROI_set_proc
  self.tROI_calling_obj = tROI_calling_obj

  ;; Register our event handler.  Make sure we have a unique name,
  ;; since we will be inherited into other objects
  self.plotwin_obj->register_forward, $
     {method:'tROI_plotwin_event', obj:self}, /draw_button_events, /draw_motion_events

  ;; If we made it here, we have successfully set up our widget.
  return, 1

end

pro pfo_tROI_obj__define

  ;; Make sure our system variables are defined for all of the
  ;; routines in this file
  init = {tok_sysvar}
  init = {pfo_sysvar}

  objectClass = $
     {pfo_tROI_obj, $
      button	: 0B, $ ;; bitmask for which mouse button is currently being pressed
      direction : 0B, $ ;; direction in which zoom region is going to grow
      tROI_set_proc: '', $ ;; callback method name used to pass tROI back to
      tROI_calling_obj: obj_new(), $ ;; calling obj (usually self)
      inherits pfo_plotwin_cw_obj}
end

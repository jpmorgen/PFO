;+
; NAME: pfo_plotwin_obj
;
; PURPOSE: Define common property and methods which are used in
; pfo_*_plotwin_obj objects (objects that control PFO widgets which
; display plots)
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
; $Id: pfo_plotwin_obj__define.pro,v 1.3 2011/09/22 23:48:07 jpmorgen Exp $
;
; $Log: pfo_plotwin_obj__define.pro,v $
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

;; Event handler --> working on this
function pfo_plotwin_cw_obj::event, event

  ;; Swallow event
  return, !tok.nowhere
end

;; Methods that return useful IDs
function pfo_plotwin_obj::drawID
  return, self.drawID
end

;; Resize method, called from event handler of tlb: draw widget X and
;; Y might need to be calculated relative to other contents in the
;; tlb.  Raw call to widget_control in tlb event handler works too,
;; but then the xsize and ysize property of this object are out of sync
function pfo_plotwin_obj::resize, xsize=xsize, ysize=ysize
  if N_elements(xsize) ne 0 then $
     self.xsize = xsize
  if N_elements(ysize) ne 0 then $
     self.ysize = ysize
  widget_control, self.drawID, draw_xsize=self.xsize, draw_ysize=self.ysize
end

;; Plot method.  This is the general plot method for the object.  The
;; calling routine tells it which pfo_plot_obj to use and which
;; pfo_obj to plot into that.  Default is to use the pfo_obj
;; encapsulated in this object and the pfo_plot_obj encapsulated in
;; that pfo_obj.  In other words, the default is to just plot the
;; pfo_obj that was handed to this object when it was initilized
pro pfo_plotwin_obj::plot, $
   pfo_obj=pfo_obj_in, $        ;; pfo_obj encapsulating information to plot
   pfo_plot_obj=pfo_plot_obj_in ;; pfo_plot_obj encapsulating information about how to plot the object

  ;; Handle pfo_debug level.  CATCH errors if _not_ debugging
  if !pfo.debug le 0 then begin
     CATCH, err
     if err ne 0 then begin
        CATCH, /CANCEL
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'ERROR: caught the above error.  Returning with what I have done so far.', /CONTINUE
        return
     endif
  endif ;; not debugging

  ;; Default pfo_obj is the one encapsulated here.  Do this in a way
  ;; as to not write pfo_obj into the calling code
  if N_elements(pfo_obj_in) ne 0 then $
     pfo_obj = pfo_obj_in
  if N_elements(pfo_obj) eq 0 then $
     pfo_obj = self.pfo_obj
  if N_elements(pfo_plot_obj_in) ne 0 then $
     pfo_plot_obj = pfo_plot_obj_in
  if N_elements(pfo_plot_obj) eq 0 then $
     pfo_plot_obj = self.pfo_obj

  ;; Plot to our draw widget.  The draw widget window_index is not
  ;; assigned until we are realized, so make sure we are...
  widget_control, self.drawID, /realize
  widget_control, self.drawID, get_value=window_index
  pfo_plot_obj->plot, pfo_obj=pfo_obj, window_index=window_index
  ;; Set IDL's active window_index back to an open current regular
  ;; window (i.e. not a draw widget window).  If there is no such
  ;; window, it stays -1
  wset, !tok.nowhere

end

;; Cleanup method.  This gets called when our plotwin_obj is dieing.
;; This can be either when the widget is killed or when the pfo is
;; dying and cleaning up registered plotwin_objs in
;; pfo_obj_plotwin_obj::cleanup
pro pfo_plotwin_obj::cleanup
  ;; Take ourselves off of the plotwin list
  self.pfo_obj->unregister_plotwin_obj, self
  
  ;; Call our inherited cleaup routines which will kill the pfo_obj if
  ;; we created it.
  self->pfo_cw_obj::cleanup

end

;; Init method
function pfo_plotwin_obj::init, $
   parentID, $ ;; widgetID of parent widget
   pfo_obj=pfo_obj, $	;; Encapsulates parinfo that will be displayed (optional)
   xsize=xsize, $ ;; X size of plot window (default = 640)
   ysize=ysize, $ ;; Y size of plot window (default = 512)
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
  
  ;; Handle our keywords and property
  if N_elements(xsize) eq 0 then xsize = 640
  if N_elements(ysize) eq 0 then ysize = 512
  self.xsize = xsize
  self.ysize = ysize

  ;; Create pfo_obj on the fly if none provided.  This makes sure the
  ;; pfo_plotwin registration/unregistration and pfo_cw_obj
  ;; registration/killing stuff always works
  if N_elements(pfo_obj) eq 0 then begin
     message, /INFORMATIONAL, 'NOTE: creating pfo_obj.  If this is not what you expect, make sure you pass me a pfo_obj.  Use pfo_quiet to suppress message.'
     ;; Just include the minimal piece of pfo_obj that we need to make
     ;; registration and unregistration work.
     self.pfo_obj = obj_new('pfo_plotwin_obj_vestigial_pfo_obj')
     self.created_pfo_obj = 1
  endif else begin
     ;; Copy the pfo_obj object reference into own property.  This
     ;; allows our methods to call pfo_obj methods.  This isn't quite
     ;; the same as inheriting all of the stuff in the pfo_obj, but it
     ;; is the next best thing.
     self.pfo_obj = pfo_obj
  endelse

  ;; If we don't have a parent, we want to create one in a
  ;; generic base
  if N_elements(parentID) eq 0 then begin
     parentID = $
        pfo_generic_base(title='PFO GENERIC PLOT WINDOW', realize=0, pfo_obj=self.pfo_obj, $
                         xsize=self.xsize, ysize=self.ysize, /tlb_size_events, _EXTRA=extra)
     created_parentID = 1
  endif ;; creating a parentID

  ;; Call our inherited init routine.  Pass the pfo_obj we may have
  ;; just created.
  ok = self->pfo_cw_obj::init(parentID, $
                              pfo_obj=self.pfo_obj, $
                              xsize=xsize, $
                              ysize=ysize, $
                              _EXTRA=extra)
  if NOT ok then return, 0

  ;; Make sure IDL does the backing store in the window with retain=2
  self.drawID = widget_draw(self.tlbID, scr_xsize=self.xsize, scr_ysize=self.ysize, $
                            retain=2, _EXTRA=extra)

  ;; Register with our pfo_obj
  self.pfo_obj->register_plotwin_obj, self

  ;; If we created our parent, realize it
  if keyword_set(created_parentID) then begin
     widget_control, parentID, realize=1
  endif ;; created parent

  ;; If we made it here, we have successfully set up our plotwin.  
  return, 1

end

;; Vestigial pfo_obj object class definition in case we were not
;; passed a pfo_obj
pro pfo_plotwin_obj_vestigial_pfo_obj::cleanup
  self->pfo_obj_cw_obj::cleanup
  self->pfo_obj_plotwin_obj::cleanup
end

function pfo_plotwin_obj_vestigial_pfo_obj::init
  ok = self->pfo_obj_cw_obj::init()
  if NOT ok then return, 0
  ok = self->pfo_obj_plotwin_obj::init()
  if NOT ok then return, 0
  return, 1
end

pro pfo_plotwin_obj_vestigial_pfo_obj__define
  objectClass = $
     {pfo_plotwin_obj_vestigial_pfo_obj, $
      inherits pfo_obj_cw_obj, $
      inherits pfo_obj_plotwin_obj}
end

;; Object class definition.
pro pfo_plotwin_obj__define
  objectClass = $
     {pfo_plotwin_obj, $
      xsize	:	0, $ ;; X size of plot window
      ysize	:	0, $ ;; Y size of plot window
      drawID	:	0L, $;; ID of draw widget
      inherits pfo_cw_obj}   ;; This is going to be a standard pfo_cw object
end

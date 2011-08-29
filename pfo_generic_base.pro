;+
; NAME: pfo_generic_base
;
; PURPOSE: Create a generic base widget for PFO compound widgets.
;
; CATEGORY: PFO widgets
;
; CALLING SEQUENCE: pfo_generic_base, (keyword arguments to
; widget_base, including things like title), containerID=containerID,
; cw_obj=cw_obj, help=help)

; DESCRIPTION: Creates a generic top-level widget using the pfo_cw
; system.  The widget has a menu bar that lets the user exit and
; display help.  The title of the widget, and the contents of the help
; are configurable

; INPUTS:
;
; OPTIONAL INPUTS:
;
; KEYWORD PARAMETERS:

;   pfo_obj (optional): pfo_obj with which this widget is associated.
;   When the pfo_obj is destroyed, this widget will be destroyed.
;   If no pfo_obj is specified, a vestigial one will be created, which
;   will be killed when this widget is killed.

;   cw_obj (output): object that runs this cw

;   containerID (optional output): If used as the parentID for
;   subsequent widgets, these widgets can be killed by the
;   cw_obj->clear_container method.  A new cw_obj->containerID() is
;   then available for repopulating the widget
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
; $Id: pfo_generic_base.pro,v 1.1 2011/08/29 18:31:33 jpmorgen Exp $
;
; $Log: pfo_generic_base.pro,v $
; Revision 1.1  2011/08/29 18:31:33  jpmorgen
; Initial revision
;
;-

;; Event to make sure scrollbars are refreshed --> this doesn't quite
;; work right in twm: event.y seems to include twm frame.
function pfo_generic_base_cw_obj::resize, event

  widget_control, event.top, update=0
  widget_control, event.ID, scr_xsize=event.x, scr_ysize=event.y
  widget_control, event.top, update=1

  return, !tok.nowhere

end

;; Cleanup method
pro pfo_generic_base_cw_obj::cleanup
  ;; Call our inherited cleaup routines
  self->pfo_cw_obj::cleanup
  ;; Free our pointers
  ptr_free, self.phelp
end

;; Init method
function pfo_generic_base_cw_obj::init, $
   title=title, $	;; title string
   x_scroll_size=x_scroll_size, $ ;; size of scrolling area (widget sizes appropriately)
   y_scroll_size=y_scroll_size, $ ;; size of scrolling area (widget sizes appropriately)
   pfo_obj=pfo_obj, $
   $ ;; For nicer display, set realize=0 and issue the command widget_control, /realize, ID in the 
   $ ;; calling code after the widget is filled
   realize=realize, $ 
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

  ;; Default title
  if N_elements(title) eq 0 then $
     title = 'PFO GENERIC BASE'

  ;; Default scroll sizes
  if N_elements(x_scroll_size) eq 0 then x_scroll_size = 640
  if N_elements(y_scroll_size) eq 0 then y_scroll_size = 512

  ;; Make sure scroll sizes aren't bigger than the screen.
  ;; Note: this creates and kills 2 widgets
  screen = get_screen_size() * 0.9
  x_scroll_size = screen[0] < x_scroll_size
  y_scroll_size = screen[1] < y_scroll_size

  ;; Call our inherited init routines.  This creates a top-level base
  ;; with a menu bar and puts pfo_obj into self, etc.
  ok = self->pfo_cw_obj::init( $
       title=title, $
       pfo_obj=pfo_obj, $
       /mbar, $
       x_scroll_size=x_scroll_size, $ 
       y_scroll_size=y_scroll_size, $ 
       uvalue={method: 'resize', obj:self}, $ ;; catch resize events
       help='Select Menu -> exit to exit widget', $
       _EXTRA=extra)
  if NOT ok then begin
     message, 'WARNING: pfo_cw_obj::init returned error.  Object not properly initilized'
     return, 0
  endif

  ;; Work with the menu bar
  menuID = widget_button(self.mbarID, value='Menu')
  ID = widget_button(menuID, value='Exit', $
                        uvalue={method: 'kill_tlb', obj:self})
  helpID = widget_button(self.mbarID, value='Help', /Help)
  ID = widget_button(helpID, value=*self.ptitle + ' help', $
                         uvalue={method: 'help', obj:self})

  ;; Default is to realize the widget, to make sure it displays.  As
  ;; described above, it is nicer to call this routine with realize=0
  ;; and then in the calling routine, do a "widget_control, /realize,
  ;; ID" when the widget is full
  if N_elements(realize) eq 0 then $
     realize = 1
  widget_control, realize=realize, self.tlbID
  xmanager, 'pfo_generic_base', self.tlbID, event_handler='pfo_cw_event_pro', /no_block

  ;; If we made it here, we have successfully set up our container.  
  return, 1

end

;; Object class definition
pro pfo_generic_base_cw_obj__define
  objectClass = $
     {pfo_generic_base_cw_obj, $
      inherits pfo_cw_obj}
end

function pfo_generic_base, $
   mbarID=mbarID, $ ;; (output) ID of the menubar widget
   containerID=containerID, $ ;; (output) optional parent widget of any subsequent children of this base
   cw_obj=cw_obj, $ ;; (output) the object that runs this cw
   _EXTRA=extra

  ;; Make sure our system variables are defined for all of the
  ;; routines in this file
  init = {tok_sysvar}
  init = {pfo_sysvar}

  ;; Initialize output
  cwID = !tok.nowhere
  
  ;; Create our controlling object.  By passing parentID=!tok.nowhere,
  ;; we are signaling that we want a top-level base widget with a menu bar
  cw_obj = obj_new('pfo_generic_base_cw_obj', parentID=!tok.nowhere, _EXTRA=extra)

  ;; The init method creates the widget and stores its ID in self.tlb.
  ;; Use the getID method to access it.  We return this ID, since that
  ;; is what people expect when they call a widget creation function.
  ;; What people will probably really want is the object to do the
  ;; heavy-duty control.  Default to a nonsense widgetID unless the
  ;; object creation was sucessful.
  if obj_valid(cw_obj) then begin
     cwID = cw_obj->tlbID()
     ;; Create a generic container if the user requests it.  They can
     ;; issue the create_container method with args (which are saved)
     ;; if they want something fancier
     if arg_present(containerID) or N_elements(containerID) then $
        cw_obj->create_container
     containerID = cw_obj->containerID()
     mbarID = cw_obj->mbarID()
  endif ;; valid cw_obj

  return, cwID

end

;+
; NAME: pfo_parinfo_edit
;
; PURPOSE: Create a base widget for editing parinfos
;
; CATEGORY: PFO widgets
;
; CALLING SEQUENCE: ID = pfo_parinfo_edit, (keyword arguments to
; widget_base, including things like title)

; DESCRIPTION: Creates a top-level base for editing parinfo segments
; or whole a parinfo.  Handles scroll bars, resizing and all that good
; stuff.  LIMITATION: the initial size is set on invocation.  If you
; put a bigger parinfo into your pfo_obj (and the base is displaying
; the whole parinfo), the widget does not automatically resize.

; INPUTS: See code for details.  parinfo should be passed by
; _reference_ and should be the same memory location as the parinfo
; encapsulated by pfo_obj.  See pfo_parinfo_parse for an example.
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
; $Id: pfo_parinfo_edit.pro,v 1.3 2011/09/16 13:41:08 jpmorgen Exp $
;
; $Log: pfo_parinfo_edit.pro,v $
; Revision 1.3  2011/09/16 13:41:08  jpmorgen
; Moved tlb resize event handler to cw_obj
;
; Revision 1.2  2011/09/16 11:14:18  jpmorgen
; *** empty log message ***
;
; Revision 1.1  2011/09/15 20:50:18  jpmorgen
; Initial revision
;-

;; Helper function to let us know if we have recursively rerun
;; ourselves to get scroll bars
function pfo_parinfo_edit_obj::new_cw_obj
  return, self.new_cw_obj
end

;; Repopulate method.  We get here if there has been a change that is
;; significant enough to mess with the order in which parinfo parses.
;; We cannot be sure that the idx we are using for this parinfo
;; segment point to the same parinfo elements on invocation and we
;; have no other way of revonstructing the idx, so the safest thing to
;; do is just kill the widget
pro pfo_parinfo_edit_obj::repopulate

  widget_control, self.tlbID, /destroy
  
end

;; Cleanup method
pro pfo_parinfo_edit_obj::cleanup
  ;; Call our inherited cleaup routines
  self->pfo_parinfo_cw_obj::cleanup
  ;; Free our pointers
  ptr_free, self.phelp
end

;; Init method
function pfo_parinfo_edit_obj::init, $
   parentID, $ ;; here for compatibility with pfo_cw_obj_new, but not used
   parinfo=parinfo, $ ;; parinfo, which should be the pfo_obj parinfo via _reference_
   title=title, $	;; title string
   pfo_obj=pfo_obj, $
   x_scroll_size=x_scroll_size, $ ;; experimenting with on-the-fly scrollbar decisions
   y_scroll_size=y_scroll_size, $
   $ ;; By default, widget is realized, but you may which to avoid that for some reason
   realize=realize, $ 
   $ ;; By defailt, the widget is non-blocking (other events and the IDL command line are processed).
   $ ;; You may wich to set no_block=0 to force user to finish with the widget before other things happen
   no_block=no_block, $
   _EXTRA=extra ;; ;; don't use REF_EXTRA here, as that lets things like idx get set on recursive call

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
     title = 'PFO PARINFO EDITOR'

  ;; Call our inherited init routines.  This creates a top-level base
  ;; with a menu bar and makes sure that uvalue can be set to
  ;; something user specified.  It also puts pfo_obj into self, etc.
  ok = self->pfo_parinfo_cw_obj::init( $
       title=title, $
       pfo_obj=pfo_obj, $
       x_scroll_size=x_scroll_size, $ ;; experimenting with on-the-fly scrollbar decisions
       y_scroll_size=y_scroll_size, $
       /mbar, $ ;; Give ourselves a menu bar
       /first_child, $ ;; hide cw_obj in a first child uvalue instead of tlb uvalue
       /tlb_size_events, $ ;; have IDL generate resize events
       uvalue={method: 'resize', obj:self}, $ ;; catch resize events (/first_child needed to let this work)
       help='Select Menu -> exit to exit widget', $ ;; --> this needs to get better with the help up pfo_string2array, and possibly the pfo_finfo system
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
;;  ;; Add undo to menu bar
;;  undoID = widget_button(self.mbarID, value='Undo')
;;  ID = widget_button(undoID, value='Enable undo', /checked_menu, $
;;                     uvalue={method: 'undo', obj:pfo_obj, keywords:{enable:1}})
;;  ID = widget_button(undoID, value='Undo', $
;;                     uvalue={method: 'undo', obj:pfo_obj, keywords:{undo:1}})
;;  ID = widget_button(undoID, value='Redo', $
;;                     uvalue={method: 'undo', obj:pfo_obj, keywords:{redo:1}})

  ;; Create a container into which the individal parinfo widgets
  ;; will display.  Make it a column base (see also pfo_parinfo_container_cw)
  self->create_container, /column

  ;; Check to see if we have been passed any parameters that would
  ;; invalidate our ability to just display the whole parinfo.  If
  ;; this is the case, we still need to register with the repopulation
  ;; list, but we will behave differently when the time comes: we will
  ;; need to delete ourselves since our contents will be invalid.
  if self->repopulate_ok(_EXTRA=extra) then begin
     ;; In the case where repopulation by pfo_parinfo_container_cw is
     ;; OK, pass the container we made here as the parent.  Let
     ;; pfo_parinfo_container_cw create the container that does the
     ;; repopulate and register itself with the repop list
     junk = pfo_parinfo_parse(/widget, parinfo, status_mask=!pfo.all_status, $
                              parentID=self->containerID(), $
                              pfo_obj=pfo_obj, _EXTRA=extra)
  endif else begin
     ;; If repopulation is not OK, register _this_ cw_obj with the
     ;; repopulation list.  The local repopulate method kills this
     ;; widget.  NOTE: we have to do this with pfo_obj's method rathre
     ;; than the local method, since the local method will run
     ;; repopulate_ok again!
     self.pfo_obj->register_repop, self
     ;; Call pfo_parinfo_parse with our containerID, since then it
     ;; will skip the needless creation of one.
     junk = pfo_parinfo_parse(/widget, parinfo, status_mask=!pfo.all_status, $
                              containerID=self->containerID(), $
                              pfo_obj=pfo_obj, _EXTRA=extra)
  endelse

  ;; Check to see if x_scroll_size and/or y_scroll_size have been
  ;; specified by user.  If so, skip the fancy recursive code
  if keyword_set(x_scroll_size) + keyword_set(y_scroll_size) eq 0 then begin
     ;; The user hasn't specified x and/or y scroll sizes.  Do so for
     ;; them.  The goal is to always have scroll bars available for a
     ;; resize, but to have the window big enough so that you don't
     ;; need them unless the window would extend off the screen
     

     ;; Get screen size
     device, get_screen_size=screen
     ;; Get widget size
     geo = widget_info(self.tlbID, /geometry)

     x_scroll_new = (screen[0] * 0.8) < geo.scr_xsize
     y_scroll_new = (screen[1] * 0.8) < geo.scr_ysize

     ;; This call to pfo_parinfo_edit is going to result in the widget
     ;; we really want.  Capture the new cw_obj so that we can replace
     ;; the one we started.  We cannot destroy that cw_obj in its init
     ;; method, otherwise this would be a little easier.
     ID = pfo_parinfo_edit(parinfo=parinfo, title=title, pfo_obj=pfo_obj, $
                           realize=realize, $
                           x_scroll_size=x_scroll_new, $
                           y_scroll_size=y_scroll_new, $
                           cw_obj=self.new_cw_obj, _EXTRA=extra)
     ;; Truncate the init of our first instance of cw_obj a little early
     return, 1
  endif ;; on-the-fly scroll bars

  ;; If we made it here, the caller (either external or internal)
  ;; specified scroll bars

  ;; Default is to realize the widget, to make sure it displays.  You
  ;; may wish to wait to do that with a "widget_control, ID, /realize"
  ;; where ID is the return value of the function that invokes this
  ;; object.
  if N_elements(realize) eq 0 then $
     realize = 1
  widget_control, realize=realize, self.tlbID
  ;; Default is to raise a non-blocking widget
  if N_elements(no_block) eq 0 then $
     no_block = 1
  xmanager, 'pfo_parinfo_edit', self.tlbID, event_handler='pfo_cw_event_pro', no_block=no_block

  ;; If we made it here, we have successfully set up our widget.  
  return, 1

end

;; Object class definition
pro pfo_parinfo_edit_obj__define
  objectClass = $
     {pfo_parinfo_edit_obj, $
      repopulate_ok	:	0B, $ ;; flag indicating whether we can repopulate or if we need to kill our tlb
      new_cw_obj	:	obj_new(), $ ;; cw_obj from second time around if we need scroll bars
      inherits pfo_parinfo_cw_obj}
end

function pfo_parinfo_edit, $
   mbarID=mbarID, $ ;; (output) ID of the menubar widget
   containerID=containerID, $ ;; (output) optional parent widget of any subsequent children of this base
   cw_obj=cw_obj, $ ;; (output) the object that runs this cw
   _REF_EXTRA=extra

  ;; Make sure our system variables are defined for all of the
  ;; routines in this file
  init = {tok_sysvar}
  init = {pfo_sysvar}

  ;; Initialize output
  cwID = !tok.nowhere
  
  ;; Create our controlling object.  No parentID means that we want a
  ;; top-level base widget with a menu bar
  cw_obj = pfo_cw_obj_new(_EXTRA=extra)

  ;; See if we recursively ran ourselves to get scroll bars on.  If
  ;; so, destroy our object and widget which never realized and
  ;; replace the output object with the one we created.
  new_cw_obj = cw_obj->new_cw_obj()
  if obj_valid(new_cw_obj) then begin
     obj_destroy, cw_obj
     cw_obj = new_cw_obj
  endif

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

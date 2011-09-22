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
; $Id: pfo_parinfo_edit.pro,v 1.4 2011/09/22 01:42:51 jpmorgen Exp $
;
; $Log: pfo_parinfo_edit.pro,v $
; Revision 1.4  2011/09/22 01:42:51  jpmorgen
; About to delete some commented out code
;
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
   parinfo=parinfo, $ 	;; parinfo, which should be the pfo_obj parinfo via _reference_
   title=title, $	;; title string
   menus=menus, $	;; list of menus to be used with pfo_menubar (default: 'pfo_generic')
   pfo_obj=pfo_obj, $	;; pfo_obj is optional.  One will be created if not provided
   x_scroll_size=x_scroll_size, $ ;; experimenting with on-the-fly scrollbar decisions
   y_scroll_size=y_scroll_size, $
   _EXTRA=extra ;; don't use REF_EXTRA here, as that lets things like idx get set on recursive call

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
  ;; with a menu bar and makes sure that the resize event can be
  ;; handled.  It also puts pfo_obj into self or creates it if none
  ;; was specified
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
  
  ;; Set up the default menus for the parinfo editor
  if N_elements(menus) eq 0 then $
     menus = ['pfo_generic', 'pfo_parinfo_edit']
  ;; Insert the menu bar menus into the menu bar.  Help is always the
  ;; last menu
  self->mbar, menus

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
     ;; OK, pass the container we made here as the *parent*.  Let
     ;; pfo_parinfo_container_cw create the container that does the
     ;; repopulate and register itself with the repop list
     self.pfo_obj->parinfo_edit, status_mask=!pfo.all_status, $
                                 parentID=self->containerID(), $
                                 _EXTRA=extra
;;     junk = pfo_parinfo_parse(/widget, parinfo, status_mask=!pfo.all_status, $
;;                              parentID=self->containerID(), $
;;                              pfo_obj=pfo_obj, _EXTRA=extra)
  endif else begin
     ;; If repopulation is not OK, register _this_ cw_obj with the
     ;; repopulation list.  The local repopulate method kills this
     ;; widget.  NOTE: we have to do this with pfo_obj's method rather
     ;; than the local method, since the local method will run
     ;; repopulate_ok again!
     self.pfo_obj->register_repop, self
     ;; Call pfo_parinfo_parse with our containerID, since then it
     ;; will skip the needless creation of one.
     self.pfo_obj->parinfo_edit, status_mask=!pfo.all_status, $
                                 containerID=self->containerID()
;;     junk = pfo_parinfo_parse(/widget, parinfo, status_mask=!pfo.all_status, $
;;                              containerID=self->containerID(), $
;;                              pfo_obj=pfo_obj, _EXTRA=extra)
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
     pfo_parinfo_edit, parinfo=parinfo, $       ;; parinfo, which should be the pfo_obj parinfo via _reference_
                       title=title, $   ;; title string
                       menus=menus, $   ;; list of menus to be used with pfo_menubar (default: 'pfo_generic')
                       pfo_obj=pfo_obj, $       ;; 
                       x_scroll_size=x_scroll_new, $ ;; experimenting with on-the-fly scrollbar decisions
                       y_scroll_size=y_scroll_new, $
                       $ ;; By default, widget is realized, but you may which to avoid that for some reason
                       realize=realize, $ 
                       $ ;; By defailt, the widget is non-blocking (other events and the IDL command line are processed).
                       $ ;; You may wich to set no_block=0 to force user to finish with the widget before other things happen
                       no_block=no_block, $
                       /recursive_call, $ ;; signal pfo_parinfo_edit that we are not done yet
                       cw_obj=self.new_cw_obj, $ ;; Store cw_obj of created widget so we can delete first-time through obj
                       _EXTRA=extra
  endif ;; on-the-fly scroll bars
  
  ;; If we made it here, we have successfully set up the widget (with
  ;; or without scrollbars) but not realized it.  IDL can't kill
  ;; an object during its init method, so we have to run the widget
  ;; from the calling code (pfo_parinfo_edit).  We also check there to
  ;; see if we need to kill the first instance of the widget that was
  ;; used to figure out our scroll bars.
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

pro pfo_parinfo_edit, $
   $ ;; parinfo (optional): array that defines the pfo function.  
   $ ;; If pfo_obj exists outside of pfo_parinfo_edit, parinfo should be passed via _reference_ as parinfo=*self.pparinfo
   $ ;; If pfo_obj is not specified in any way by the caller, the widget is blocking and, after editing, parinfo is returned and the pfo_obj is destroyed
   $ ;; If pfo_obj is present as a keyword but undefined, the pfo_obj created here is returned
   parinfo=parinfo, $ ;; (optional)
   pfo_obj=pfo_obj, $ ;; (optional) pfo_obj encapsulting parinfo and widget stuff
   mbarID=mbarID, $ ;; (output) ID of the menubar widget
   containerID=containerID, $ ;; (output) optional parent widget of any subsequent children of this base
   cw_obj=cw_obj, $ ;; (output) the object that runs this cw
   $ ;; By default, widget is realized, but you may which to avoid that for some reason
   realize=realize_in, $ ;;  be polite to calling code
   $ ;; By defailt, the widget is non-blocking (other events and the IDL command line are processed).
   $ ;; You may wish to set no_block=0 to force user to finish with the widget before other things happen.  
   $ ;; When pfo_obj is created on the fly, no_block is always set to 0
   no_block=no_block_in, $ ;;  be polite to calling code
   recursive_call=recursive_call, $ ;; signal that this is a call from within the pfo_parinfo_edit_obj init method
   _EXTRA=extra ;; all other arguments are passed to and from primitives with the _EXTRA mechanism

  ;; Make sure our system variables are defined for all of the
  ;; routines in this file
  init = {tok_sysvar}
  init = {pfo_sysvar}

  ;; Initialize output
  cwID = !tok.nowhere
  
  ;; Check to see if the user supplied a pfo_obj but forgot to supply
  ;; a parinfo.  Ideally, the parinfo in the pfo_obj (*self.pparinfo)
  ;; and parinfo are the same memory area.  This can only happen if
  ;; pfo_parinfo_edit was called from within the pfo_obj with code
  ;; something like this:
  ;;ID = pfo_obj->call_function( $
  ;;     /no_update, 'pfo_parinfo_parse', /widget, group_leader=self.tlbID, no_block=0, $
  ;;     /no_finit, menus=['pfo_finit', 'pfo_parinfo_edit'])
  if N_elements(parinfo) eq 0 and obj_valid(pfo_obj) then begin
     N_parinfo = pfo_obj->parinfo_call_function(/no_update, 'N_elements')
     if N_parinfo ne 0 then begin
        message, 'WARNING: _Copying_ parinfo out of pfo_obj.  If you have a pfo_obj, you should really '
     endif ;; some parinfo in pfo_obj
  endif ;; no parinfo supplied


  ;; Handle no_block in a polite way to the caller
  if N_elements(no_block_in) ne 0 then $
     no_block = no_block_in

  ;; Create pfo_obj on the fly if none provided.  
  if N_elements(pfo_obj) eq 0 or NOT obj_valid(pfo_obj) then begin
     message, /INFORMATIONAL, 'NOTE: creating pfo_obj.  If this is not what you expect, make sure you pass pfo_obj.  Use pfo_quiet to suppress message.'
     ;; Make a basic pfo_obj to make parinfo editing work.
     pfo_obj = pfo_obj_new()
     created_pfo_obj = 1
     ;; _Copy_ our parinfo into the pfo_obj since we depend on it
     ;; being defined both inside and outside the pfo_obj.  For this
     ;; reason it is preferable to call this from within the parinfo
     pfo_obj->set_property, parinfo_array=parinfo
     ;; Make sure we are a blocking widget
     if keyword_set(no_block) then $
        message, /CONTINUE, 'NOTE: forcing no_block=0 so parinfo can be captured and returned'
     no_block = 0
  endif ;; creating pfo_obj on the fly

  ;; Create our widget controlling object.  No parentID means that we want a
  ;; top-level base widget with a menu bar
  cw_obj = pfo_cw_obj_new(parinfo=parinfo, pfo_obj=pfo_obj, no_block=no_block, _EXTRA=extra)

  ;; See if there was any problem in the cw_obj creation
  if obj_valid(cw_obj) and NOT keyword_set(recursive_call) then begin
     ;; Successful cw_obj creation
     ;; Get our tlb
     cwID = cw_obj->tlbID()
     ;; Get our containerID, in case caller wants to put more stuff in
     containerID = cw_obj->containerID()
     ;; Get our mbarID in case caller wants to add more menus (better
     ;; to do with menus keyword, so help ends up in the right order)
     mbarID = cw_obj->mbarID()
     ;; See if we recursively run ourselves to get scroll bars on.  If
     ;; so, destroy the first instance of our object and widget which
     ;; never realized and replace the output object with the one we
     ;; created.
     new_cw_obj = cw_obj->new_cw_obj()
     if obj_valid(new_cw_obj) then begin
        obj_destroy, cw_obj
        cw_obj = new_cw_obj
     endif ;; second-time object creation for scroll bars

     ;; Default is to realize the widget, to make sure it displays.  You
     ;; may wish to wait to do that with a "widget_control, ID, /realize"
     ;; where ID is the return value of the function that invokes this
     ;; object.
     ;; Handle realize in a polite way to the caller
     if N_elements(realize_in) ne 0 then $
        realize = realize_in
     if N_elements(realize) eq 0 then $
        realize = 1
     widget_control, realize=realize, cwID

     ;; Default is to raise a non-blocking widget
     if N_elements(no_block) eq 0 then $
        no_block = 1
     xmanager, 'pfo_parinfo_edit', cwID, event_handler='pfo_cw_event_pro', no_block=no_block

  endif ;; valid cw_obj

  ;; Handle the case where we created our pfo_obj
  if keyword_set(created_pfo_obj) then begin
     ;; We are going to want the parinfo from the pfo_obj.  If the
     ;; user doesn't want the pfo_obj, move the parinfo out.
     ;; Beware the case of undefined parinfo
     N_parinfo = pfo_obj->parinfo_call_function(/no_update, 'N_elements')
     if N_parinfo ne 0 then $
        parinfo = pfo_obj->parinfo(no_copy=~arg_present(pfo_obj))     
     ;; Get rid of pfo_obj to prevent memory leaks, unless the user
     ;; really wants it
     if NOT arg_present(pfo_obj) then $
        obj_destroy, pfo_obj
  endif ;; cleaning up creating pfo_obj

end

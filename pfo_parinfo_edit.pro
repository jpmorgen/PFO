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
; $Id: pfo_parinfo_edit.pro,v 1.6 2011/11/18 16:04:00 jpmorgen Exp $
;
; $Log: pfo_parinfo_edit.pro,v $
; Revision 1.6  2011/11/18 16:04:00  jpmorgen
; Significant work on repopulate stuff to try to keep widgets fast
;
; Revision 1.5  2011/09/22 23:45:28  jpmorgen
; Some improvement to recursive stuff
;
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
pro pfo_parinfo_edit_obj::repopulate

  N_parinfo = self.pfo_obj->parinfo_call_function(/no_update, 'N_elements')
  ;; If there are no parinfo left, destroy this widget.  I can argue
  ;; it either way, but it makes a little more sense that we should
  ;; not leave a widget up that is displaying a subset of nothing.
  ;; Hopefully there is a widget up that allows the user to add to the
  ;; parinfo.
  if N_parinfo eq 0 then begin
     widget_control, self.tlbID, /destroy
     return
  endif ;; no parinfo to display

  ;; If we made it here, there is some parinfo in the pfo_obj.  See if
  ;; there are any parameters that we recognize.  Get the uniqueIDs
  self.pfo_obj->parinfo_call_procedure, $
     /no_update, 'pfo_struct_setget_tag', /get, taglist_series='pfo_unique', uniqueID=uniqueID
  ;; Get the order in which they parse
  new_indices = self.pfo_obj->indices(status_mask=!pfo.all_status)

  ;; Loop through the original uniqueIDs, expanding any matches with
  ;; the new uniqueIDs, building up the new list of idx (new_idx),
  ;; which we will edit.
  for ioID=0, N_elements(*self.puniqueID)-1 do begin
     ;; Check to see if the original parameter is still in the
     ;; parinfo.  match_idx is an index into the new parinfo
     match_idx = where((*self.puniqueID)[ioID] eq uniqueID, count)
     if count eq 0 then $
        CONTINUE

     ;; If we made it here, our original parameter is still in the
     ;; parinfo.  Check to see if it is already in our accumulating
     ;; list of new_idx (e.g. it was added during an expansion of an
     ;; earliear parameter in this function)
     if N_elements(new_idx) ne 0 then begin
        junk = where((*self.puniqueID)[ioID] eq uniqueID[new_idx], count)
        ;; No need to add more than once
        if count ne 0 then $
           CONTINUE
     endif ;; have some new_idx to check

     ;; If we made it here, our old parameter is still in the parinfo
     ;; but is not in the list of new_idx yet.  Expand the function it
     ;; lives in and put all of the parameters of that function in our
     ;; new_idx list
     pfo_array_append, new_idx, $
                       self.pfo_obj->indices(expand_idx=match_idx, $
                                             status_mask=!pfo.all_status)

  endfor ;; each original uniqueID

  ;; Check to see if any of our old parameters are still in the parinfo
  if N_elements(new_idx) eq 0 then begin
     widget_control, self.tlbID, /destroy
     return
  endif ;; no parameters to display

  ;; If we made it here, we have a new set of parameters to display.
  ;; Let the user decide if they really want to

  ;; This is reasonably fast, so don't bother user each time
  ;;ok = dialog_message('Redisplay ' + *self.ptitle + '?', /question, dialog_parent=self.tlbID)
  ;;if ok eq 'No' then  begin
  ;;   widget_control, self.tlbID, /destroy
  ;;   return
  ;;endif ;; user doesn't want to repopulate

  ;; If we made it here, we are really going to redisplay.  Its too
  ;; bad that we can't "break in" to the displayed widgets and
  ;; change their idx to the proper values so the already running
  ;; widget can display the right info.  But that is not how I
  ;; designed things.

  ;; Refresh our idx and uniqueID list
  *self.pidx = temporary(new_idx)
  self.pfo_obj->parinfo_call_procedure, $
     /no_update, 'pfo_struct_setget_tag', /get, taglist_series='pfo_unique', $
     idx=*self.pidx, uniqueID=*self.puniqueID

  ;; Turn off update in the widget heirarchy so we don't unnecessarily
  ;; redraw widgets.  Even with this in place, this ends up being slow
  ;; in Windows.  It is even slower if update repains 1
  widget_control, self.tlbID, update=0

  ;; Kill the container and all its contents.  Each of the children
  ;; should properly issue pfo_obj->unregister_refresh.  This also
  ;; creates a fresh container into which we will draw the new version
  ;; of the widget
  self->clear_container

  self.pfo_obj->parinfo_edit, idx=*self.pidx, status_mask=!pfo.all_status, $
         containerID=self.containerID, _EXTRA=*self.pextra

  ;; Redraw the widget hierarchy
  widget_control, self.tlbID, update=1

end

;; Cleanup method
pro pfo_parinfo_edit_obj::cleanup
  ;; Call our inherited cleaup routines
  self->pfo_parinfo_cw_obj::cleanup
  ;; Local cleanup
  ptr_free, self.puniqueID
end

;; Init method
function pfo_parinfo_edit_obj::init, $
   parentID, $ ;; here for compatibility with pfo_cw_obj_new, but not used
   parinfo=parinfo, $ 	;; parinfo, which should be the pfo_obj parinfo via _reference_
   title=title, $	;; title string
   menus=menus, $	;; list of menus to be used with pfo_menubar (default: 'pfo_generic')
   menu_args=menu_args, $ ;; optional keyword argument(s) to the *_menu widget functions
   pfo_obj=pfo_obj, $	;; pfo_obj is optional.  One will be created if not provided
   x_scroll_size=x_scroll_size, $ ;; Scroll bars are frequently necessary, but don't make the user specify them
   y_scroll_size=y_scroll_size, $ ;; We will do a recursive call to set them based on widget size and display size
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

  ;; Call our inherited init routines.  Use the pfo_parinfo_cw/pfo_cw
  ;; system to to create a top-level base with a menu bar and makes
  ;; sure that resize events can be handled.  Registration of this
  ;; cw_obj in the pfo_obj and some other useful common tasks are also
  ;; handled.

  ;; Make sure we keep the arguments to widget_base in
  ;; pfo_cw_obj::init separate from the _EXTRA arguments which will
  ;; eventually go to pfo_parinfo_parse and points beyond.  The
  ;; pfo_cw_obj encapsulates the _EXTRA so we can reuse it when we
  ;; repopulate, but we don't want the widget_base arguments in
  ;; there, since then we get funny things like nested scroll bars
  widget_base_args = $
     {uvalue : {method: 'resize', obj:self}, $ ;; catch resize events (/first_child needed to let this work)
      tlb_size_events : 1 $ ;; have IDL generate resize events
      }

  ;; We want to always enabled scroll bars.  We will do this with some
  ;; fancy recursive work
  if N_elements(x_scroll_size) ne 0 then $
     pfo_struct_append, widget_base_args, {x_scroll_size : x_scroll_size}
  if N_elements(y_scroll_size) ne 0 then $
     pfo_struct_append, widget_base_args, {y_scroll_size : y_scroll_size}

  ok = self->pfo_parinfo_cw_obj::init( $
       title=title, $
       pfo_obj=pfo_obj, $
       /mbar, $ ;; Give ourselves a menu bar
       /first_child, $ ;; hide cw_obj in a first child uvalue instead of tlb uvalue
       help='Select Menu -> exit to exit widget', $ ;; --> this needs to get better with the help up pfo_string2array, and possibly the pfo_finfo system
       widget_base_args=widget_base_args, $ ;; args specifically for widget_base in cw_obj
       _EXTRA=extra)
  if NOT ok then begin
     message, 'WARNING: pfo_cw_obj::init returned error.  Object not properly initilized'
     return, 0
  endif

  ;; Initialize our local property
  self.puniqueID = ptr_new(/allocate_heap)

  ;; Set up the default menus for the parinfo editor
  if N_elements(menus) eq 0 then $
     menus = ['pfo_generic', 'pfo_parinfo_edit']
  ;; Insert the menu bar menus into the menu bar.  Help is always the
  ;; last menu
  self->mbar, menus, menu_args=menu_args

  ;; Create a container into which the individal parinfo widgets
  ;; will display.  Make it a column base (see also pfo_parinfo_container_cw)
  self->create_container, /column

  ;; Check to see if we have been passed any parameters that would
  ;; invalidate our ability to just display the whole parinfo.  If
  ;; this is the case, we can still redisplay our subset of the
  ;; parinfo, but we have to work harder.
  if self->repopulate_ok(_EXTRA=extra) then begin
     ;; In the case where repopulation by the simple method in
     ;; pfo_parinfo_container_cw is OK, pass the container we made
     ;; here as the *parent*.  This prevents us from being a recursive
     ;; call and lets pfo_parinfo_container_cw, called from
     ;; pfo_parinfo_parse, create the container that does the
     ;; repopulate and register itself with the repop list
     self.pfo_obj->parinfo_edit, status_mask=!pfo.all_status, $
                                 parentID=self->containerID(), $
                                 _EXTRA=extra
  endif else begin
     ;; A simple redisplay of _all_ of the parinfo will not work.  
     ;; Prepare to use pfo_unique.uniqID to help repopulate our
     ;; widget in an intelligent manner.

     ;; Save the uniqueIDs for this segment of parinfo
     N_parinfo = self.pfo_obj->parinfo_call_function(/no_update, 'N_elements')
     if N_parinfo gt 0 then begin
        ;; Make sure we have the pfo_unique structure and that it is
        ;; updated, but don't update anything else
        self.pfo_obj->parinfo_call_procedure, $
           /no_update, 'pfo_parinfo_update', required_tags='pfo_unique', $
           update_only_tags='pfo_unique', pfo_obj=self.pfo_obj
        ;; Grab just our segment of uniqueIDs, again making sure
        ;; we don't unnecessarily update anything else (e.g. widgets)
        self.pfo_obj->parinfo_call_procedure, $
           /no_update, 'pfo_struct_setget_tag', /get, taglist_series='pfo_unique', $
           idx=*self.pidx, uniqueID=*self.puniqueID
     endif ;; some parinfo to work with

     ;; Register _this_ cw_obj in the pfo_obj's repop list.  NOTE: we
     ;; have to do this with pfo_obj's method rather than the local
     ;; method, since the local method will run repopulate_ok again!
     self.pfo_obj->register_repop, self
     ;; Call the parinfo_edit method with our containerID as the
     ;; *containerID.* This will prevent a recursive call back here
     ;; and skip the needless creation of another container by
     ;; pfo_parinfo_container_cw.
     self.pfo_obj->parinfo_edit, idx=*self.pidx, status_mask=!pfo.all_status, $
                                 containerID=self->containerID(), $
                                 _EXTRA=extra
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
                       x_scroll_size=x_scroll_new, $ ;; calculated optimum widget size
                       y_scroll_size=y_scroll_new, $
                       /recursive_call, $ ;; signal pfo_parinfo_edit that we are not done yet
                       cw_obj=new_cw_obj, $ ;; Store cw_obj of created widget so we can delete first-time through obj
                       _EXTRA=extra
     ;; Can't store into a structure tag unless you do it this way
     self.new_cw_obj = new_cw_obj
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
      puniqueID		:	ptr_new(), $ ;; pointer to array of pfo_unique.uniqueIDs for repopulation
      inherits pfo_parinfo_cw_obj}
end

pro pfo_parinfo_edit, $
   $ ;; parinfo (optional): array that defines the pfo function.  
   $ ;; If pfo_obj exists outside of pfo_parinfo_edit, parinfo should be passed via _reference_ as parinfo=*self.pparinfo
   $ ;; If pfo_obj is not specified in any way by the caller, the widget is blocking and, after editing, parinfo is returned and the pfo_obj is destroyed
   $ ;; If pfo_obj is present as a keyword but undefined, the pfo_obj created here is returned
   parinfo=parinfo, $ ;; (optional: see above)
   pfo_obj=pfo_obj, $ ;; (optional: see above)
   mbarID=mbarID, $ ;; (output) ID of the menubar widget
   containerID=containerID, $ ;; (output) optional parent widget of any subsequent children of this base
   cw_obj=cw_obj, $ ;; (output) the object that runs this cw
   $ ;; By default, widget is realized, but you may which to avoid that for some reason
   realize=realize_in, $ ;;  be polite to calling code
   $ ;; By default, the widget is non-blocking (other events and the IDL command line are processed).
   $ ;; You may wish to set no_block=0 to force user to finish with the widget before other things happen.  
   $ ;; When pfo_obj is created on the fly, no_block is always set to 0
   no_block=no_block_in, $ ;;  be polite to calling code
   recursive_call=recursive_call, $ ;; signal that this is a call from within the pfo_parinfo_edit_obj init method
   _REF_EXTRA=extra ;; all other arguments are passed to and from primitives with the _REF_EXTRA mechanism

  ;; Make sure our system variables are defined for all of the
  ;; routines in this file
  init = {tok_sysvar}
  init = {pfo_sysvar}

  ;; For debugging purposes, make sure we are running in
  ;; object-oriented mode only, politely saving off old state of flag
  oobjects_only = !pfo.objects_only
  !pfo.objects_only = 1

  ;; Handle pfo_debug level.  CATCH errors if _not_ debugging
  if !pfo.debug le 0 then begin
     ;; Return to the calling routine with our error
     ON_ERROR, !tok.return
     CATCH, err
     if err ne 0 then begin
        CATCH, /CANCEL
        ;; Remember to restore old state of objects_only
        !pfo.objects_only = oobjects_only
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'USAGE: pfo_parinfo_edit, parinfo=parinfo, pfo_obj=pfo_obj, mbarID=mbarID, containerID=containerID, cw_obj=cw_obj, realize=realize, no_block=no_block, _EXTRA=extra'
     endif
  endif ;; not debugging

  ;; Handle no_block in a polite way to the caller, since we may
  ;; change it
  if N_elements(no_block_in) ne 0 then $
     no_block = no_block_in

  ;; Create pfo_obj on the fly if none provided.  
  if NOT obj_valid(pfo_obj) then begin
     ;; --> change this when I get a file menu with write capabilities
     if NOT (arg_present(pfo_obj) or arg_present(parinfo) or N_elements(parinfo) ne 0) then $
        message, 'ERROR: pfo_obj and/or parinfo are not specified.  No way to capture output.  Consider using pfo_fit instead.'
     message, /INFORMATIONAL, 'NOTE: creating pfo_obj.  If this is not what you expect, make sure you pass pfo_obj.  Use pfo_quiet to suppress message.'
     ;; Make a basic pfo_obj to make parinfo editing work.  Note that
     ;; pfo_fit allows you to specify the name of the object class you
     ;; really want to use
     pfo_obj = pfo_obj_new()
     ;; Move our parinfo into the pfo_obj to save memory and time.
     pfo_obj->set_property, parinfo_array=parinfo, /no_copy
     ;; Make sure we are a blocking widget if the user expects output
     if (arg_present(pfo_obj) or arg_present(parinfo) or N_elements(parinfo) ne 0) then begin
        if keyword_set(no_block) then $
           message, /CONTINUE, 'NOTE: forcing no_block=0 so parinfo and/or pfo_obj can be captured and returned (if possible -- IDL XMANAGER cannot block if the first widget was non-blocking)'
        no_block = 0
     endif ;; adjusting no_block to wait for return of parinfo and/or pfo_obj
     ;; Call the edit_parinfo method of our new pfo_obj.  This ends up
     ;; calling us recursively, but since pfo_obj is valid, we use the
     ;; else, below.
     pfo_obj->parinfo_edit,    $
        mbarID=mbarID, $ ;; (output) ID of the menubar widget
        containerID=containerID, $ ;; (output) optional parent widget of any subsequent children of this base
        cw_obj=cw_obj, $ ;; (output) the object that runs this cw
        $ ;; By default, widget is realized, but you may which to avoid that for some reason
        realize=realize_in, $ ;;  be polite to calling code
        no_block=no_block, $ ;; Make sure we are non-blocking, if possible
        _EXTRA=extra

     ;; We are going to want the parinfo from the pfo_obj.  If the
     ;; user doesn't want the pfo_obj, move the parinfo out.
     ;; Otherwise, copy it.  Beware the case of undefined parinfo.
     ;; Note, this ends up nuking the parinfo in the functioning
     ;; editor if it tried to block but couldn't
     N_parinfo = pfo_obj->parinfo_call_function(/no_update, 'N_elements')
     if N_parinfo ne 0 then $
        parinfo = pfo_obj->parinfo(no_copy=~arg_present(pfo_obj))     
     ;; Get rid of pfo_obj to prevent memory leaks, unless the user
     ;; really wants it
     if NOT arg_present(pfo_obj) then $
        obj_destroy, pfo_obj

  endif else begin
     
     ;; If we made it here, we have the "normal" case, where this code is
     ;; invoked with a pfo_obj and its encapsulated parinfo (e.g. with
     ;; pfo_obj->parinf_edit)

     ;; Create our widget controlling object.  We don't specify a
     ;; parentID, which means will get a top-level base widget with a
     ;; menu bar.  We are going to use some fancy recursive code in
     ;; the init method to get the scroll bars set up.  We will sort
     ;; that out below.  Pass all _input_ command line arguments so
     ;; everything propagates through.  Output command line arguments
     ;; will be constructed below.
     cw_obj = pfo_cw_obj_new( $
              parinfo=parinfo, $ ;; If we made it here, parinfo and pfo_obj are defined
              pfo_obj=pfo_obj, $ ;; 
              $ ;; By default, widget is realized, but you may which to avoid that for some reason
              realize=realize_in, $ ;;  be polite to calling code
              $ ;; By defailt, the widget is non-blocking (other events and the IDL command line are processed).
              $ ;; You may wish to set no_block=0 to force user to finish with the widget before other things happen.  
              $ ;; When pfo_obj is created on the fly, no_block is always set to 0
              no_block=no_block_in, $ ;;  be polite to calling code
              _EXTRA=extra) ;; all other arguments are passed to and from primitives with the _EXTRA mechanism

     ;; See if there was any problem in the cw_obj creation.  If we are a
     ;; recursive call, return without realizing the widget so our
     ;; init method can see how big the widget turned out.
     if obj_valid(cw_obj) and NOT keyword_set(recursive_call) then begin
        ;; If we made it here, we have a valid cw_obj _from our first
        ;; (maybe only) call to init_.  If we called ourselves
        ;; recursively from the init method to set up scroll bars, we
        ;; have hidden the cw_obj of the widget we really want in
        ;; new_cw_obj
        new_cw_obj = cw_obj->new_cw_obj()
        if obj_valid(new_cw_obj) then begin
           ;; We called ourselves recursively to set up scroll bars.
           ;; Destroy the first cw_obj, which was never realized and
           ;; replace it with the second one we created, which has
           ;; properly sized scroll bars.
           obj_destroy, cw_obj
           cw_obj = new_cw_obj
        endif ;; second-time object creation for scroll bars

        ;; Get our tlb
        cwID = cw_obj->tlbID()
        ;; Get our containerID, in case caller wants to put more stuff in
        containerID = cw_obj->containerID()
        ;; Get our mbarID in case caller wants to add more menus (better
        ;; to do with menus keyword, so help ends up in the right order)
        mbarID = cw_obj->mbarID()

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

  endelse ;; creating pfo_obj vs. using an existing one

  ;; Remember to restore old state of objects_only
  !pfo.objects_only = oobjects_only

  end

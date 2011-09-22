;+
; NAME: pfo_cw_obj__define

; PURPOSE: Handles top-level widget creation and management for pfo
; widgets.
;
; CATEGORY: PFO widgets
;
; CALLING SEQUENCE:

; DESCRIPTION: This is the general workhorse of the PFO widget
; system.  It can create:

; a top-level base (with or without a menubar, with or without a
; "first child" and with or without a "container base")

; a top-level base that is the child of another widget (parentID
; positional argument to init method, again with or without first
; child and/or "container")

; a top-level button for a menu to be inserted into an mbar

; In all cases, a compound widget controlling object, cw_obj, is
; created and stored in the uvalue of either the top-level widget or
; its first child.  This cw_obj is also "registered" with a list of
; cw_obj maintained by pfo_obj_cw_obj (inherited into the pfo_obj).
; That means that widgets created by the object will be kill when the
; object is destroyed.

; True top-level bases look nice with:

;       /mbar, $ ;; Give ourselves a menu bar
;       /first_child, $ ;; hide cw_obj in a first child uvalue instead of tlb uvalue
;       /tlb_size_events, $ ;; have IDL generate resize events
;       uvalue={method: 'resize', obj:self}, $ ;; catch resize events (/first_child needed to let this work)
;       help='Detailed help'

; For true top-level bases or other multi-widget applications, you
; will need to use the create_container method and pass the necessary
; arguments to it, like /column, /row, etc. to have that base arrange
; the multiple widgets that you put into it.  The widget ID
; self.containerID should then be the parent of your child widgets.
; There is also a clear_container method, which allows you to delete
; all children widgets created by your inheriting object.  This is
; handy for "repopulate" methods (see pfo_parinfo_cw_obj__define.pro).

; EVENTS: the object-oriented even handling system was inspired by
; David Fanning's fsc_field compound widget.  The basic idea is to put
; a structure in the uvalue of the widget that is generating the
; event.  The structure, which is created from within this object, or
; an object that inherits this object, looks like one of the
; following:

; uvalue={method:'method_name', obj:self}
; uvalue={method:'method_name', obj:self, keywords:{keyword1:1, keyword2:1}})

; The required pieces of the structure are the name of the method and
; the object on which it will be called (e.g. self->method_name).
; Keywords are optionally passed as a separte structure using IDL's
; _EXTRA mechanism.  The method needs to be a function that accepts
; one positional parameter, the original event (see, e.g.,
; pfo_cw_obj::kill_tlb).  The event handling method can, in principle
; be in any object (e.g. pfo_obj), but for organizational reasons, it
; is recommended that the events be kept with the object that inherits
; this code.

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

; Requires textlineformat from David Fanning's Coyote IDL library

;
; PROCEDURE:
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;
; $Id: pfo_cw_obj__define.pro,v 1.5 2011/09/22 23:50:17 jpmorgen Exp $
;
; $Log: pfo_cw_obj__define.pro,v $
; Revision 1.5  2011/09/22 23:50:17  jpmorgen
; Add menus, generic resize
;
; Revision 1.4  2011/09/16 13:39:46  jpmorgen
; Put tlb resize event method here.  Improve documentation
;
; Revision 1.3  2011/09/16 11:31:14  jpmorgen
; Allow streamlining of widgets so child and container are optional
;
; Revision 1.2  2011/09/03 15:30:03  jpmorgen
; ABout to experiment with no child, but having everything in parent to
; see if that speeds things up
;
; Revision 1.1  2011/09/01 22:12:51  jpmorgen
; Initial revision
;
;-

;; Methods that return useful IDs
function pfo_cw_obj::parentID
  return, self.parentID
end

function pfo_cw_obj::tlbID
  return, self.tlbID
end

function pfo_cw_obj::mbarID
  return, self.mbarID
end

function pfo_cw_obj::containerID
  return, self.containerID
end

;; Common event-oriented methods:

;; Kill_tlb.  This kills the entire widget hierarchy.
function pfo_cw_obj::kill_tlb, event
  ;; The tlb top of this event will be the help window
  widget_control, event.top, /destroy
end

;; Resize event in case we are a tlb.  This is necessary to make sure
;; sure that our scoll bars track with the window size in UNIX.  If we
;; don't have scrollbars in our widget, this method should be
;; overridden.
function pfo_cw_obj::resize, event

  ;; Prepare to swallow the event
  retval = !tok.nowhere

  sn = tag_names(event, /structure_name)
  if sn ne 'WIDGET_BASE' then begin
     message, 'WARNING: received an event of type "' + sn + '" which I do not know how to deal with.  Swallowing event.', /CONTINUE
     return, retval
  endif

  ;; Adjusting the screen size has different effects in the two
  ;; windowing systems I use.
  if !d.name eq 'X' then begin
     ;; I subtract a few pixels in X windows, since, at least for twm,
     ;; the twm top decorations are included in the reported size
     widget_control, event.ID, scr_xsize=event.x, scr_ysize=event.y-33
  endif else begin
     ;; In Windows, xsize and ysize are in tune with the event x and y
     widget_control, event.ID, xsize=event.x, ysize=event.y
  endelse

  return, retval

end

;; HELP method for this cw_obj.  This is called by the cw_obj event
;; handler.  It raises a dialog box and displayes the help string
;; encapsulated in this object.  Because a separate cw_obj is formed
;; for each compound widget in the PFO system, each one can have its
;; own help string.  Because the help string is property of the
;; cw_obj, it can be changed, although no provision is made to
;; redisplay the contents of a help window that is already open.  Be
;; recursive and use our own cw_obj system to make the help display
;; widget.
function pfo_cw_obj::help, event

  ;; Create a cw_obj for the help window.  Don't make this
  ;; window a modal widget.  Modal widget allows a keypress
  ;; (e.g. escape or kp-enter) to dismiss the widget (see
  ;; cancel_button, default_button keywords to widget_control).  But
  ;; blocks action on other widgets.  I like to have the help up while
  ;; I do other things.
  cw_obj = obj_new('pfo_cw_obj', $
                   group_leader=event.top, $
                   /floating, $ ;; Make this float above calling widget
                   /column, $
                   title=*self.ptitle + ' help', $
                   help='Help widget can remain open for reference.  Hit "done" button to dismiss.')

  if NOT obj_valid(cw_obj) then begin
     a = dialog_message('% PFO_CW_OBJ::HELP: ERROR: the cw_obj system did not return a valid object.  This should not happen!  Use pfo_debug and/or see console window to debug')
  endif ;; invalid cw_obj

  ;; Make our help a text widget.  Put it into the container, just in
  ;; case we eventually want to do fancy refreshing
  tlbID = cw_obj->tlbID()
  ;; textlineformat is from David Fanning's Coyote
  ;; library.  It does a line wrap
  ID = widget_text(tlbID, $
                   /scroll, $
                   value=textlineformat(*self.phelp, length=80), $
                   xsize=85, ysize=24)
  ;; Use our object-oriented event handler.  Note, this seems to work
  ;; if we use self (cw_obj of invoking widget) or cw_obj, since the
  ;; method just queries the event structure.  We might not always be
  ;; that lucky, so make sure we are carefuly with obj
  doneID = widget_button(tlbID, value='Done', $
                         uvalue={method: 'kill_tlb', obj:cw_obj})
  ;; Only useful for modal widget
  ;;widget_control, tlbID, $
  ;;                cancel_button=doneID, default_button=doneID
  widget_control, tlbID, /realize
  xmanager, *self.ptitle, tlbID, event_handler='pfo_cw_event_pro', /no_block

end

;; The mbar method creates the menu bar.  calls the widget functions in the menu_list (appending
;; _menu) in order to 
pro pfo_cw_obj::mbar, $
   menu_list, $ ;; vector string of top-level menu widgets to insert into the self.mbar (see *_menu.pro)
   menu_args=menu_args ;; optional keyword argument(s) to the *_menu widget functions

  ;; Always make sure there is a menu item to exit the widget
  if N_elements(menu_list) eq 0 then $
     ID = pfo_generic_menu(self.mbarID, pfo_obj=self.pfo_obj)

  ;; Cycle through any menu_list items
  for im=0, N_elements(menu_list)-1 do begin
     ;; Handle pfo_debug level.  CATCH errors if _not_ debugging
     if !pfo.debug le 0 then begin
        CATCH, err
        if err ne 0 then begin
           CATCH, /CANCEL
           message, /NONAME, !error_state.msg, /CONTINUE
           message, 'ERROR: caught the above error.  Skipping menu ' + menu_list[im], /CONTINUE
           CONTINUE
        endif
     endif ;; not debugging
     ID = call_function(menu_list[im] + '_menu', self.mbarID, pfo_obj=self.pfo_obj, _EXTRA=menu_args)
  endfor ;; each menu
  
  ;; Always add help.  Hopefully, the user has passed a decent help
  ;; string into the widget.  The /help makes the help menu appear on
  ;; the far right side of the menubar in UNIX, but has no effect in
  ;; Windows.
  helpID = widget_button(self.mbarID, value='Help', /Help)
  ID = widget_button(helpID, value=*self.ptitle + ' help', $
                         uvalue={method: 'help', obj:self})

end

;; Default refresh method.  Catch cases where people have forgotten to
;; define their own
pro pfo_cw_obj::refresh
  help, self, output=s
  message, /CONTINUE, 'WARNING: refresh method for ' + s + ' not specified'
end

;; Local register refresh method.  Allows consistent syntax in
;; inherited routines
pro pfo_cw_obj::register_refresh
  self.pfo_obj->register_refresh, self
end

;; Create_container method.  We make a container into which the rest
;; of our children will be displayed.  This allows us to more easily
;; destroy and remake the widget.  Events, where necessary, are going
;; to be handled by the object-oriented method suggested by David
;; Fanning, and exemplified in fsc_field.  --> I might want to have
;; some property that keeps track of the base alignment.  For now,
;; left is what I want for everything.
pro pfo_cw_obj::create_container, $
   _EXTRA=extra ;; arguments to widget base.  These will be encapsulated, if supplied

  ;; Replace our encapsulated container args with any [new] ones
  ;; passed here
  if N_elements(extra) ne 0 then $
     *self.pcontainer_args = extra

  ;; By default, put our container directly under the tlb.  This
  ;; streamlines things
  parentID = self.tlbID
  ;; If we have a valid first child, put the container under that so
  ;; that we can clear out the tlb uvalue and event_* keywords
  if widget_info(self.childID, /valid_id) then $
     parentID = self.childID  

  ;; Create our container with the encapsulated arguments
  self.containerID = $
     widget_base(parentID, $
                 _EXTRA=*self.pcontainer_args)
end

;; clear_container method.  Kill the container widget and create it
;; anew.  This is typically called when you want to repopulate the
;; widget with a new version of your children.  Assuming you are
;; issuing this command in your cw_obj, it will fit into a sequence
;; like this:

;; Turn off update in the parent so we don't unnecessarily redraw
;; widgets
;; widget_control, self.parentID, update=0
;; self->clear_container
;; commands that create child widgets
;; Redraw the parent widget
;; widget_control, self.parentID, update=1

pro pfo_cw_obj::clear_container
  if widget_info(self.containerID, /valid_ID) then begin
     widget_control, self.containerID, /destroy
  endif else begin
     message, /CONTINUE, 'WARNING: Encapsulated containerID not valid.  Did you destroy it via some other command?  Ignoring error and just creating a new one'
  endelse
  self->create_container
end

;; Object-oriented event handler for all pfo_cw widgets.  This is
;; always the event handler of the the first child of our tlb.  If we
;; are not making use of the containerID system, the first child event
;; handler will never get events.  Instead, we will need to manually
;; assign the event handler.  NOTE: IDL documentation in widget_base
;; event_func/event_pro suggests that the top-level base event handler
;; be an argument to XMANAGER, which means it needs to be a procedure
;; (see pfo_cw_event_pro)
function pfo_cw_event_func, event
  ;; Handle pfo_debug level.  CATCH errors if _not_ debugging
  if !pfo.debug le 0 then begin
     CATCH, err
     if err ne 0 then begin
        CATCH, /CANCEL
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'ERROR: caught the above error.  Swallowing this event ', /CONTINUE
        return, !tok.nowhere
     endif
  endif ;; not debugging

  ;; Get the uvalue, which should be a structure created in the cw_obj
  ;; of our widget of the form: 
  ;; uvalue={method:'method_name', obj:self, keywords:{keyword_name:1}})
  widget_control, event.ID, get_uvalue=uvalue

  if N_elements(uvalue) eq 0 then $
     message, 'ERROR: You created a widget that produces events, but you forgot to set uvalue={method:"method_name", obj:self, keywords:{keyword_name:1}'

  ;; Call our method
  junk = where(tag_names(uvalue) eq 'KEYWORDS', count)
  if count eq 0 then $
     return, call_method(uvalue.method, uvalue.obj, event) $
  else $
     return, call_method(uvalue.method, uvalue.obj, event, _EXTRA=uvalue.keywords)

end

;; For top-level bases, we need an event handling procedure rather
;; than a function (IDL requirement, see pfo_cw_event_func doc).  The
;; exception to this would be if we are having all of our childern be
;; children of self.containerID.  In that case the event_func in the
;; container can do all the work.  Nevertheless, it is a good idea to
;; have an event handler at the top level....
pro pfo_cw_event_pro, event
  junk = pfo_cw_event_func(event)
end

;; Kill notify procedure
;; This is called when the first child is destroyed by a widget operation.
pro pfo_cw_kill, ID
  ;; Get our cw_obj out of the first child.  We can only use
  ;; widget_control at this point, otherwise I would have rather
  ;; done away with the first child and made things run faster
  widget_control, ID, get_uvalue=cw_obj
  obj_destroy, cw_obj
end

;; Cleanup method.  This gets called when our cw_obj is dieing.  This
;; can be either from the pfo_cw_kill, or when the pfo is dying and
;; cleaning up registered cw_objs in pfo_parinfo_cw_obj::cleanup
pro pfo_cw_obj::cleanup
  ;; Unregister the cw_obj to pfo_obj connection, now that we are
  ;; dying
  self.pfo_obj->unregister_cw_obj, self

  ;; Take ourselves off of the refresh list.  This doesn't cause
  ;; any problems if we didn't register in the first place
  self.pfo_obj->unregister_refresh, self

  ;; If we created pfo_obj, destroy it
  if keyword_set(self.created_pfo_obj) then $
     obj_destroy, self.pfo_obj

  ;; Free our pointers from the heap
  ptr_free, self.ptitle
  ptr_free, self.phelp
  ptr_free, self.pcontainer_args
  ptr_free, self.pextra

  ;; If our tlb is still valid, that means we are being killed from
  ;; the pfo_obj side of things.  That means that all of our displayed
  ;; widgets are still going to be up with no guts to drive them,
  ;; unless we kill them here.
  if widget_info(self.tlbID, /valid_ID) then $
     widget_control, self.tlbID, /destroy

end

;; Init method
function pfo_cw_obj::init, $
   parentID, $ 		;; Parent widget ID.  If not specified, or !tok.nowhere, a top-level base is created
   pfo_obj=pfo_obj, $	;; Encapsulates parinfo that will be displayed (optional)
   first_child=first_child , $ ;; create a first child in which the self object is stored.  This clears room for a user-defined uvalue at the tlb.  Most pfo widgets don't need this kind of first child to work properly and they work more slowly if there is one
   mbar=mbar, $		;; If present and widget will be a top-level base, is the widgetID of the menu bar (see IDL widget_base documentation)
   menu=menu, $		;; /menu indicates widget will be the top menu button suitable for insertion into an mbar
   title=title, $	;; title string (for top-level base widgets and tab widgets)
   tab_mode=tab_mode, $ ;; by default, tab_mode=1, so tabbing works
   help=help, $		;; Help string for this cw
   _EXTRA=extra 	;; Capture extra keywords in the *self.pextra property.  
  $			;; Note, these also include any _EXTRA to widget_base, at least for now

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

  ;; Initialize our property

  ;; Capture title input for use in help window
  self.ptitle = ptr_new(/allocate_heap)
  if N_elements(title) ne 0 then $
     *self.ptitle = title

  ;; Create pfo_obj on the fly if none provided.  This makes sure the
  ;; pfo_cw_obj registration/killing stuff always works
  if N_elements(pfo_obj) eq 0 then begin
     message, /INFORMATIONAL, 'NOTE: creating pfo_obj.  If this is not what you expect, make sure you pass pfo_obj.  Use pfo_quiet to suppress message.'
     ;; Just include the minimal piece of pfo_obj that we need to make
     ;; registration and unregistration work.
     self.pfo_obj = obj_new('pfo_obj_cw_obj')
     self.created_pfo_obj = 1
  endif else begin
     ;; Copy the pfo_obj object reference into own property.  This
     ;; allows our methods to call pfo_obj methods.  This isn't quite
     ;; the same as inheriting all of the stuff in the pfo_obj, but it
     ;; is the next best thing.
     self.pfo_obj = pfo_obj
  endelse

  ;; Register with the cw_obj list so that we can be killed if the
  ;; pfo_obj is killed.  This, of course is only important if pfo_obj
  ;; is supplied by caller
  self.pfo_obj->register_cw_obj, self

  ;; Parent of this widget.  If none provided, this widget will be a
  ;; top-level base
  self.parentID = !tok.nowhere
  if N_elements(parentID) ne 0 then $
     self.parentID = parentID

  ;; Just in case this is the first use of the windowing system and
  ;; the user hasn't set things up in the .Xresources file, we want to
  ;; get into TrueColor mode, which is common between X and MS
  ;; Windows.  We deal with decomposed colors and all of that stuff
  ;; when we plot
  if !d.name eq 'X' then $
     device, true_color=24

  ;; Make our conception of the tlb of this widget as simple as
  ;; possible.  If the user wants the tlb to look different, any valid
  ;; keywords to widget_base can be passed via _EXTRA.  --> I might
  ;; eventually want to have a separate widget_base_args so as to not
  ;; confuse this with the _EXTRA that are encapsulated above, but for
  ;; now, it is convenient to lump them all together.

  ;; Default is no mbar
  self.mbarID = !tok.nowhere

  ;; Here is the code that figures out what kind of tlb to create: a
  ;; genuine tlb (with or without a menu bar), a tlb that has a parent
  ;; or a top-level button in a menu

  ;; Respond to our flag that the cw is for a top-level widget
  if self.parentID eq !tok.nowhere then begin
     ;; top-level widget case.  Check to see if user erroneously
     ;; specified /menu and didn't provide a parentID
     if keyword_set(menu) then $
        message, 'ERROR: /menu is not a valid keyword when creating a top-level base.  Did you mean /mbar?'
     ;; See if we want a menu bar
     if arg_present(mbar) or N_elements(mbar) ne 0 then begin
        self.tlbID = $
           widget_base(title=title, mbar=mbar, _EXTRA=extra)
        self.mbarID = mbar
     endif else begin
        ;; Just a plain base with no menu bar
        self.tlbID = widget_base(title=title, _EXTRA=extra)
     endelse ;; mbar or not
  endif else begin
     ;; Check to see if we are creating a regular compound widget or
     ;; the top button of a menu
     if keyword_set(menu) then begin
        ;; This will raise an error if self.parentID is not an mbar
        ;; widgetID.        
        self.tlbID = widget_button(self.parentID, _EXTRA=extra)
     endif else begin
        ;; Regular compound widget case
        self.tlbID = widget_base(self.parentID, _EXTRA=extra)
     endelse
  endelse

  ;; Tab mode works what I think of as the right way by default in
  ;; UNIX.  We need tab_mode=1 for Windows, unless, of course, the
  ;; user doesn't want it.  NOTE: tab mode needs to be avoided in menus
  if N_elements(tab_mode) eq 0 and NOT keyword_set(menu) then $
     tab_mode = 1
  widget_control, self.tlbID, tab_mode=tab_mode

  ;; A proper compund widget (which you can get with the /first_child
  ;; switch) has a base into which the user can write their own uvalue
  ;; and can intercept set_value with a pro_set_value.  But most PFO
  ;; widgets don't need all of that stuff and adding the layer of a
  ;; first child multiplied the amount of time it took to repopulate
  ;; widgets, particularly on Windows machines.  So by default, just
  ;; keep as much as possible in our tlb.  If you want to have the
  ;; first child scheme, just specify /first_child.  

  ;; See if we are going to create a first child or not
  if keyword_set(first_child) then begin
     ;; In the first child way of doing things, the tlb is left as
     ;; empty as possible.  All subsequent widgets should be the
     ;; children of the first child so that the events are sure to be
     ;; handled by the pfo_cw_event_func.  If you don't get the
     ;; parentage right, hopefully the call to xmanager for the very
     ;; most tlb has event_handler='pfo_cw_event_pro'
     self.childID = widget_base(self.tlbID, $
                                event_func='pfo_cw_event_func', $
                                uvalue=self, kill_notify='pfo_cw_kill')

  endif else begin
     ;; By default, we streamline things by leaving out the first
     ;; child.  Note that the kill_notify must be associated with the
     ;; widget that has uvalue set to the cw_obj
     self.childID = !tok.nowhere
     widget_control, self.tlbID, $
                     set_uvalue=self, event_func='pfo_cw_event_func', $
                     kill_notify='pfo_cw_kill'
  endelse

  ;; Let the user create_container with their own arguments.  This
  ;; lets us encapsulate these arguments for use later, if necessary
  self.containerID = !tok.nowhere
  
  ;; Default help
  self.phelp = ptr_new('No help available')
  if N_elements(help) ne 0 then $
     *self.phelp = help

  ;; Prepare to encapsulate artuments to widget_base in
  ;; create_container method.
  self.pcontainer_args = ptr_new(/allocate_heap)

  ;; Save our _EXTRA arguments as a courtesy to the inheriting object.
  ;; This allows the resulting cw_obj to recreate the conditions under
  ;; which it was invoked for proper redisplay.  NOTE: these in
  ;; principle could conflict with the _EXTRA being passed to the call
  ;; to widget_base, below.
  self.pextra = ptr_new(/allocate_heap)
  if N_elements(extra) ne 0 then $
     *self.pextra = extra

  ;; If we made it here, we have successfully set up our container.
  ;; It is up to the inheriting routines to do the rest.
  return, 1

end

;; Object class definition
pro pfo_cw_obj__define
  objectClass = $
     {pfo_cw_obj, $
      parentID	:	0L, $ ;; widget ID of parent.  !tok.nowhere if this is a top-level base
      tlbID	:	0L, $ ;; top level base of this widget
      childID	:	0L, $ ;; widget ID of first child, created with /first_child switch to init
      mbarID	:	0L, $ ;; menu bar widget ID, if a top-level base, !tok.nowhere otherwise
      containerID:	0L, $ ;; base into which all child widgets will be deposited (first child of first child)
      pfo_obj	:	obj_new(), $ ;; pfo_obj encapsulating PFO data, parinfo, etc.
      created_pfo_obj : 0, $ ;; we create a vestigial pfo_obj on the fly if none is provided
      ptitle	: 	ptr_new(), $ ;; title string (for top-level base widgets and tab widgets)
      phelp	: 	ptr_new(), $ ;; help string for this cw
      pcontainer_args:	ptr_new(), $ ;; args to widget_base used to create container
      pextra:	ptr_new() $ ;; EXTRA arguments encapsulated for possible use in repopulate
     }
end

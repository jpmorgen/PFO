;+
; NAME: pfo_plotwin_obj__define

; PURPOSE: Define the object which controls pfo plot windows

; CATEGORY: PFO widgets
;
; CALLING SEQUENCE:

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
; $Id: pfo_plotwin_obj__define.pro,v 1.4 2011/11/18 15:52:13 jpmorgen Exp $
;
; $Log: pfo_plotwin_obj__define.pro,v $
; Revision 1.4  2011/11/18 15:52:13  jpmorgen
; Extensive work with event handler, get unregistration to work properly
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

;; Useful methods that return property
function pfo_plotwin_obj::last_pfo_plot_obj
  return, self.last_pfo_plot_obj
end

function pfo_plotwin_obj::last_pfo_obj
  return, self.last_pfo_obj
end

;; Event handler.  This basically just forwards event to the list of
;; event handlers maintained in the linked list: self.forward_obj.
;; The items stored in the linked list resemble the uvalues of
;; pfo_cws: item = {method:method, obj:cw_obj, keywords:{k0:v0, k1:v1...}}
function pfo_plotwin_obj::pfo_plotwin_event, event

  retval = !tok.nowhere

  ;; Loop through each node in the linked list
  repeat begin
     ;; Catch errors, which are presumably improperly formatted 
     ;; Handle pfo_debug level.  CATCH errors if _not_ debugging
     if !pfo.debug le 0 then begin
        CATCH, err
        if err ne 0 then begin
           CATCH, /CANCEL
           message, /NONAME, !error_state.msg, /CONTINUE
           message, 'ERROR: caught the above error.  Did you register your forward entry properly?  Should have been registered as {method:method, obj:cw_obj, [keywords:{keywords}]}.  Skipping this node.', /CONTINUE
           if ptr_valid(node) then $
              node = (*node).next
           CONTINUE
        endif
     endif ;; not debugging

     ;; Make sure our forward list has been initialized
     if NOT obj_valid(self.forward_obj) then $
        return, retval
     ;; Make sure there are event handlers registered in our forward
     ;; list
     if self.forward_obj->get_count() eq 0 then $
        return, retval

     ;; If we made it here, we have a valid forward list

     ;; Initilize our node pointer the first time through
     if NOT ptr_valid(node) then $
        node = self.forward_obj->get_node(0)

     junk = where(tag_names((*(*node).item)) eq 'KEYWORDS', count)
     if count eq 0 then $
        junk = call_method((*(*node).item).method, (*(*node).item).obj, event) $
     else $
        junk = call_method((*(*node).item).method, (*(*node).item).obj, event, _EXTRA=(*(*node).item).keywords)

     
     ;; Check to see if the last event we procesed kill our object
     ;; (e.g. ctrl-d)
     if ptr_valid(node) then $
        node = (*node).next

  endrep until NOT ptr_valid(node)

  ;; Swallow event
  return, retval
end

;; This even handler is registered in the pfo_plotwin object's forward
;; list when the group_leader keyword of the init method is set.  It
;; kills the group_leader widget when ctrl-d is pressed.  Of course,
;; the init method of this object must also be called with the
;; keyboard_events keyword set.  NOTE: NO USER PROMPTING IS DONE!  The
;; group leader is abruptly killed.  The user may wish to override
;; this method by not providing a group leader argument to
;; pfo_plotwin, but rather setting up a similar system in the parent
;; widget which provides the appropriate prompts.
function pfo_plotwin_obj::ctrld, $
   event

  ;; Do sanity check on origin of event
  sn = tag_names(event, /structure_name)
  if sn ne 'WIDGET_DRAW' then $
     message, 'ERROR: expecting a WIDGET_DRAW event, got ' + sn

  ;; Only handle keyboard events
  if event.type eq !tok.char then begin
     ;; Only respond to ctrl-d.  Also, don't generate an error
     ;; if our group leader is not around
     if event.ch eq 4 and widget_info(self.group_leader, /valid_id) then $
        widget_control, self.group_leader, /destroy
  endif

  
end

;; Unregister an event hander from our forward list.
pro pfo_plotwin_obj::unregister_forward, $
   eh ;; event handler reference of the form {method:'method', obj:obj, [keywords:{k1:k1, k2:k2}]} -OR- obj for which all event handlers will be deleted

  ;; Quietly return if there are no cw_objs in our list, since people
  ;; are unlikely to check to see if they were registered in the first
  ;; place
  if NOT obj_valid(self.forward_obj) then $
     return

  ;; Catch errors, which are presumably improperly formatted 
  ;; Handle pfo_debug level.  CATCH errors if _not_ debugging
  if !pfo.debug le 0 then begin
     CATCH, err
     if err ne 0 then begin
        CATCH, /CANCEL
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'ERROR: caught the above error.  Did you specify your event handler properly?  {method:method, obj:cw_obj, [keyowrds:{keywords}]}.', /CONTINUE
        return
     endif
  endif ;; not debugging

  ;; Check syntax of invocation
  case size(/type, eh) of 
     !tok.struct : begin
        obj = eh.obj
        method = eh.method
     end
     !tok.objref : obj = eh
  endcase

  if NOT obj_valid(obj) then $
     message, 'ERROR: event handler obj is not valid'

  ;; Loop through each node in the linked list.  Do this as a while,
  ;; since the list shrinks as we delete things.
  in = 0
  while in lt self.forward_obj->get_count() do begin
     ;; Catch errors, which are presumably improperly formatted 
     ;; Handle pfo_debug level.  CATCH errors if _not_ debugging
     if !pfo.debug le 0 then begin
        CATCH, err
        if err ne 0 then begin
           CATCH, /CANCEL
           message, /NONAME, !error_state.msg, /CONTINUE
           message, 'ERROR: caught the above error.  Did you register your forward entry properly?  Should have been registered as {method:method, obj:cw_obj, [keyowrds:{keywords}]}.  Skipping this node.', /CONTINUE
           CONTINUE
        endif
     endif ;; not debugging

     node = self.forward_obj->get_item(in)
     ;; Check to see if we have arrived at our cw_obj
     if (*node).obj eq obj then begin
        ;; We need to delete unless...
        delete_me = 1
        ;; ...the method is specified and doesn't match
        if keyword_set(method) then $
           if (*node).method ne method then $
              delete_me = 0
        if keyword_set(delete_me) then begin
           self.forward_obj->delete, in
           ;; prepare to try this node number again, since the ones on
           ;; top will have compressed to fill this space 
           in -=1
        endif ;; deleting a node
     endif ;; obj matches
     in += 1
  endwhile ;; each node list forward list

end

;; Register a cw_obj in our forward list.  
pro pfo_plotwin_obj::register_forward, $
   eh, $ ;; event handler
   _REF_EXTRA=extra ;; args to pass to widget_control of the drawID (e.g. to turn on appropriate event generation)

  ;; Catch errors, which are presumably improperly formatted 
  ;; Handle pfo_debug level.  CATCH errors if _not_ debugging
  if !pfo.debug le 0 then begin
     CATCH, err
     if err ne 0 then begin
        CATCH, /CANCEL
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'ERROR: caught the above error.  Did you specify your event handler properly?  {method:method, obj:cw_obj, [keyowrds:{keywords}]}.', /CONTINUE
        return
     endif
  endif ;; not debugging


  ;; Check syntax of invocation
  if NOT obj_valid(eh.obj) then $
     message, 'ERROR: event handler obj is not valid'

  ;; Store forward list in Coyote programs linkedlist object
  if NOT obj_valid(self.forward_obj) then $
     self.forward_obj = obj_new('linkedlist')

  ;; Pass any switches to widget_control to turn on appropriate event
  ;; handler capabilities
  if keyword_set(extra) then $
     widget_control, self.drawID, _EXTRA=extra

  ;; Store our forward list in Coyote programs linkedlist object
  self.forward_obj->add, eh

end

;; Methods that return useful IDs
function pfo_plotwin_obj::drawID
  return, self.drawID
end

;; Event handler for tlb resize for the generic tlb we created here.
;; Adapted from pfo_generic_base.  If plotwin is used in another tlb,
;; its resize method should look something like this, but calculate
;; what the xsize and ysize args to pfo_plotwin_obj::resize relative
;; to the widgets in that tlb.
function pfo_plotwin_obj::resize, event

  ;; Prepare to swallow the event
  retval = !tok.nowhere

  ;; Check to see if type of event is valid.  Resize events are of type WIDGET_BASE
  sn = tag_names(event, /structure_name)
  if sn ne 'WIDGET_BASE' then begin
     message, 'WARNING: received an event of type "' + sn + '" which I do not know how to deal with.  Swallowing event.', /CONTINUE
     return, retval
  endif

  ;; Check to make sure we are only being used as the resize event
  ;; handler for the generic tlb created here
  if event.ID ne self.tlb_cw_obj->tlbID() then $
     message, 'ERROR: this event handler should only be used by the generic tlb created by this object.  Do you mean to call this object''s resize *procedure* to resize the plotwin?'

  ;; In LINUX, at least with twm, the twm top decorations are included
  ;; in the reported event size.  This is not as advertised in IDL.
  ;; Fix it at this level.
  if !d.name eq 'X' then $
     event.y -= 33

  self->pfo_plotwin_obj::resize, xsize=event.x, ysize=event.y

  ;; Issue the replot method in the pfo_obj.  This assumes the generic
  ;; case that this pfo_plotwin is displaying the pfo_plot_obj
  ;; encapsulated in pfo_obj.  The plotwin could, in fact, be
  self.pfo_obj->replot

  ;; Swallow event
  return, !tok.nowhere

end

;; Plotwin resize method (*procedure*), called from an event handler
;; of a true tlb which handles resize events and knows how big the
;; plotwin should be.
pro pfo_plotwin_obj::resize, xsize=xsize, ysize=ysize
  ;; Track draw_widget size in the local property
  if N_elements(xsize) ne 0 then $
     self.xsize = xsize
  if N_elements(ysize) ne 0 then $
     self.ysize = ysize

  ;; Resize the draw widget.  Note that we need to use [xy]size, not
  ;; draw_[xy]size, otherwise the actual size of the window doesn't
  ;; change, even though we don't have scroll bars.
  widget_control, self.drawID, xsize=self.xsize, ysize=self.ysize

  ;; Synchronize draw_widget size with property in pfo_plot_obj in the
  ;; pfo_obj.  If you are using a pfo_plot_obj that is stored
  ;; somewhere else, you will probably have made your own
  ;; pfo_<mytask>_plotwin using this code as a template.
  self.pfo_obj->set_property, $
     plot_xsize=xsize, $
     plot_ysize=ysize

  ;; NOTE: calling code should issue the plot method, since there
  ;; might be other things that would need to be refreshed

end

;; Convert_device_coord method.  This is basically just a wrapper
;; around IDL's convert_coord which makes sure our active window (if
;; any) is selected
function pfo_plotwin_obj::convert_device_coord, $
   x, $
   y, $
   z, $
   _REF_EXTRA=extra

  ;; Check to see if anything has been plotted yet.  If not, return NANs
  if obj_valid(last_pfo_plot_obj) then $
     return, make_array(N_params(), value=!values.d_NAN)

  ;; If we made it here, we have a valid plot in our window

  ;; Get the window index
  widget_control, self.drawID, get_value=window_index
  ;; Make the window active
  wset, window_index
  ;; Call IDL's convert coords
  case N_params() of 
     1 : coords = convert_coord(x, _EXTRA=extra)
     2 : coords = convert_coord(x, y, _EXTRA=extra)
     3 : coords = convert_coord(x, y, z, _EXTRA=extra)
  endcase

  ;; Use the special argument of -1 to set IDL's active window_index
  ;; back to an open current regular window (i.e. not a draw widget
  ;; window).  If there is no such window, it stays -1 (i.e., invalid)
  wset, !tok.nowhere

  return, coords

end

;; For a (list of) points x, y on the currently displayed plot,
;; returns an n x 3 vector where the x3 correspond to: Xin, Xaxis, and
;; the input y value translated into data coodinates by IDL.  NOTE:
;; widget_draw events return event.[xy] device coordinates, so use the
;; /device keyword.
function pfo_plotwin_obj::get_pfo_coord, $
   x, $
   y, $
   data=data, $
   device=device, $
   normal=normal

  ;; Check to see if anything has been plotted yet.  If not, just
  ;; return 0s
  if NOT obj_valid(self.last_pfo_plot_obj) then $
     return, make_array(3, value=!values.d_NAN)

  ;; If we made it here, we have a valid plot in our window

  ;; Find the coordinates of x,y on the graph.  This assumes user has
  ;; properly entered "from" switch
  coords = self->convert_device_coord(x, y, data=data, device=device, normal=normal, $
                                      /to_data)

  ;; Figure out what the plot's X-axis is reading in
  self.last_pfo_plot_obj->get_property, plot_Xunits=plot_Xunits
  ;; Get Xin and Xaxis sorted out in a way that minimizes calculations
  case plot_Xunits of
     !pfo.Xin: begin
        Xin = coords[0,*]
        Xaxis = self.last_pfo_obj->Xaxis(Xin=Xin)
     end
     !pfo.Xaxis: begin
        Xaxis = coords[0,*]
        Xin = self.last_pfo_obj->convert_coord(Xaxis, /from_Xaxis, /to_Xin)
     end
     else: message, 'ERROR: invalid Xunits value: ' + strtrim(plot_Xunits, 2) + ' expecting !pfo.Xin or !pfo.Xaxis'
  endcase

  return, [Xin, Xaxis, coords[1, *]]

end

;; Plot method.  This is the general plot method for the object.  The
;; calling routine tells it which pfo_plot_obj to use and which
;; pfo_obj to plot into that.  Default is to use the pfo_obj
;; encapsulated in this object on invokation and the pfo_plot_obj
;; encapsulated in that pfo_obj.  In other words, the default is to
;; just plot the pfo_obj that was handed to this object when it was
;; initialized. --> could add set/get property methods to set pfo_obj
;; and pfo_plot_obj to other values.  Could add /last switch that uses
;; last pfo_obj and pfo_plot_obj
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

  ;; Clear out our last plot and pfo_objs before we try to plot new
  ;; ones.  This way, if we generate an error, we won't have stale
  ;; objects in there.
  self.last_pfo_plot_obj = obj_new()
  self.last_pfo_obj = obj_new()

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

  ;; Plot to our draw widget.  The plot method should generally catch
  ;; all of its own errors, so rely on the plot_error output to see if
  ;; we were successful.  The draw widget window_index is not assigned
  ;; until we are realized, so make sure we are realized
  widget_control, self.drawID, /realize
  widget_control, self.drawID, get_value=window_index
  pfo_plot_obj->plot, pfo_obj=pfo_obj, window_index=window_index, $
                      plot_error=plot_error
  ;; Use the special argument of -1 to set IDL's active window_index
  ;; back to an open current regular window (i.e. not a draw widget
  ;; window).  If there is no such window, it stays -1 (i.e., invalid)
  wset, !tok.nowhere

  ;; Check the plot_error output.  If it is set to a non-zero value,
  ;; return before we capture the last_*_obj stuff.
  if keyword_set(plot_error) then $
     return
     

  ;; If we made it here, we have successfully used the pfo_plot_obj
  ;; plot method to plot to plot information from pfo_obj.  Save these
  ;; object references since we will need them for coordinate
  ;; transformations
  self.last_pfo_plot_obj = pfo_plot_obj
  self.last_pfo_obj = pfo_obj

end

;; Cleanup method.  This gets called when our plotwin_obj is dieing.
;; This can be either when the widget is killed or when the pfo is
;; dying and cleaning up registered plotwin_objs in
;; pfo_obj_plotwin_obj::cleanup
pro pfo_plotwin_obj::cleanup
  ;; Take ourselves off of the plotwin list
  self.pfo_obj->unregister_plotwin_obj, self
  
  ;; --> Take ourselves off of our own forward list (a little silling,
  ;; but good for debugging)
  self->unregister_forward, self

  ;; Clean up our local stuff
  obj_destroy, self.forward_obj  

  ;; Call our inherited cleaup routines which will kill the pfo_obj if
  ;; we created it.
  self->pfo_cw_obj::cleanup

end

;; Init method
function pfo_plotwin_obj::init, $
   parentID, $ ;; widgetID of parent widget
   group_leader=group_leader, $ ;; (optional) widgetID to kill when ctrl-d pressed in plotwin (=0 means kill plotwin parent)
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
  ;; pfo_plotwin registration/unregistration and pfo cw_obj
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

  ;; If we don't have a parent, we want to create one in a base
  ;; with some reasonable features, like the ability to resize
  if N_elements(parentID) eq 0 then begin
     ;; Create a generic tlb
     tlbID = $
        pfo_generic_base( $
        title='PFO PLOT WINDOW', $
        realize=0, $
        pfo_obj=self.pfo_obj, $
        /no_scroll, $
        /first_child, $ ;; hide cw_obj in a first child uvalue instead of tlb uvalue
        /tlb_size_events, $ ;; generate resize events
        uvalue={method: 'resize', obj:self}, $ ;; catch resize events with our own obj -- overrides uvalue in call to pfo_cw_obj::init (/first_child also needed to let this work)
        cw_obj=cw_obj, $ ;; grab the cw_obj so we can properly resize component widgets
        _EXTRA=extra)
     created_tlbID = 1
     ;; Save our cw_obj
     self.tlb_cw_obj = cw_obj
     ;; Make our parent the first child
     parentID = self.tlb_cw_obj->childID()
     ;; Handle our group_leader=0 case
     if N_elements(group_leader) ne 0 then begin
        if group_leader eq 0 then begin
           ;; This is more of a test case for debugging, so don't mind
           ;; changing input
           group_leader = tlbID
        endif ;; group_leader eq 0 case
     endif ;; group_leader check
  endif ;; creating a parentID

  ;; Call the pfo_cw_obj init routine.  Just make a generic base,
  ;; unless the user thinks they know what they are doing and passes
  ;; some _EXTRA keywords.  WARNING: do not pass xsize and ysize at
  ;; this point, because that will force you to manage the size of the
  ;; widget as the plotwin size changes.
  ok = self->pfo_cw_obj::init(parentID, $
                              pfo_obj=self.pfo_obj, $
                              _EXTRA=extra)

  if NOT ok then return, 0

  ;; Put our draw widget into the tlb created by the cw_obj init
  ;; method.  NOTE: this tlb is _not_ the tlb created above in the
  ;; case we were invoked with no parent.  Make sure IDL does the
  ;; backing store in the window with retain=2.
  self.drawID = widget_draw(self.tlbID, $
                            xsize=self.xsize, ysize=self.ysize, $
                            retain=2, $
                            uvalue={method:'pfo_plotwin_event', obj:self}, _EXTRA=extra)

  ;; Register with our pfo_obj
  self.pfo_obj->register_plotwin_obj, self

  ;; Handle the group_leader argument
  if N_elements(group_leader) ne 0 then begin
     ;; Store property
     self.group_leader = group_leader
     ;; Register our local ctrl-d event handler and make sure the draw
     ;; window generates keyboard events.
     self->register_forward, {method:'ctrld', obj:self}, /draw_keyboard_events
  endif

  ;; If we created our parent, realize it
  if keyword_set(created_tlbID) then begin
     widget_control, tlbID, realize=1
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
      tlb_cw_obj:	obj_new(), $ ;; just in case we are creating a tlb (useful for debugging)
      xsize	:	0, $ ;; X size of plot window
      ysize	:	0, $ ;; Y size of plot window
      drawID	:	0L, $;; ID of draw widget
      last_pfo_plot_obj: obj_new(), $ ;; last pfo_plot_obj used to make a plot
      last_pfo_obj: obj_new(), $ ;; last pfo_obj used to make a plot
      forward_obj:	obj_new(), $ ;; list of obj methods w/optional keywords which will recieve events
      group_leader:	0L, $ ;; widgetID to kill when ctrl-d pressed in plotwin
      inherits pfo_cw_obj}   ;; This is going to be a standard pfo_cw object
end

;+
; NAME: pfo_fit
;
; PURPOSE: 
;   Provide the primary graphical user interface (GUI) to the
;   Parameter Function Object (PFO) system
;
; CATEGORY: PFO
;
; CALLING SEQUENCE:
;   pfo_fit[[pfo_obj_class][, Xin], Yin[, Yerr]] [, pfo_obj=pfo_obj]
;   [keyword args to underlying routines])
;
; INPUTS:
;
; OPTIONAL INPUTS:
;   pfo_obj_class: class name of pfo_obj to use instead of generic
;   pfo_obj (e.g. gpaw_obj)

;   Xin, Yin, Yerr: If just one is specified, it is assumed to be
;   Yin.  If two, Xin, Yin.  Yerr, if not specified, is set to sqrt(Yin)

; KEYWORD PARAMETERS:
;   pfo_obj: a pre-existing pfo_obj to display, or an undefined
;   variable into which to return the pfo_obj created and modified by
;   pfo_fit.  In either case, the caller should be prepared to issue
;   the obj_destroy command on pfo_obj at the apropriate time to avoid
;   memory leaks.  See README.pfo_obj for an introduction to the pfo_obj.

;   Other keyword arguments are explained in the code.  NOTE: not all
;   keywords are explicitly mentioned at each level.  For example,
;   there aren't a lot of keywords listed for pfo_fit, but
;   because the _EXTRA mechanism is used to pass keywords onto
;   pfo_fit_obj::init, any keyword valid for that routine is a valid
;   keyword for pfo_fit.  Similary pfo_fit_obj calls pfo_cw_obj::init,
;   which itself has a list of keywords.  pfo_cw_obj::init calls IDL
;   routines like widget_base which receive the _EXTRA command.

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
; $Id: pfo_fit.pro,v 2.4 2012/01/13 20:55:03 jpmorgen Exp $
;
; $Log: pfo_fit.pro,v $
; Revision 2.4  2012/01/13 20:55:03  jpmorgen
; Add help
;
; Revision 2.3  2011/12/01 22:10:43  jpmorgen
; Improve caller access to expanding menus, allow customization of enable_undo
;
; Revision 2.2  2011/11/18 14:46:06  jpmorgen
; Improve widget
;
; Revision 2.1  2011/11/03 01:47:22  jpmorgen
; About to replace with pfo_fit
;
; Revision 2.0  2011/09/22 23:54:49  jpmorgen
; Using pfo_obj and pfo_fit
;
; Revision 1.4  2011/09/22 17:45:01  jpmorgen
; Preparing to adapt to PFO of 2011
;
; Revision 1.3  2011/02/10 22:27:31  jpmorgen
; Fix bug in propagation of _EXTRA keywords!
;
; Revision 1.2  2010/12/31 21:57:13  jpmorgen
; Change error reporting a little
;
;-

;; Resize event.
function pfo_fit_obj::resize, event

  ;; Prepare to swallow the event
  retval = !tok.nowhere

  ;; Check to see if type of event is valid.  Resize events are of
  ;; type WIDGET_BASE
  sn = tag_names(event, /structure_name)
  if sn ne 'WIDGET_BASE' then begin
     message, 'WARNING: received an event of type "' + sn + '" which I do not know how to deal with.  Swallowing event.', /CONTINUE
     return, retval
  endif

  ;; Check to make sure we are only being used as the resize event
  ;; handler for the tlb created here
  if event.ID ne self.tlbID then $
     message, 'ERROR: this event handler should only be used by the pfo_fit tlb'

  ;; Copy event.[xy] into local variables
  xsize = event.x
  ysize = event.y

  ;; In LINUX, at least with twm, the twm top decorations are included
  ;; in the reported event size.  This is not as advertised in IDL.
  ;; Fix it at this level.
  if !d.name eq 'X' then $
     ysize -= 33

  ;; Adjust the [xy]size, making sure that we have a minimum width
  ;; so axis lables show
  ;;plot_xsize = 640 > xsize*self.plot_xfrac
  plot_xsize = xsize*self.plot_xfrac
  ;;ysize = 512 > ysize
  self.plotwin_obj->resize, xsize=plot_xsize, ysize=ysize

  ;; Adjust the scrolling base that contains the parinfo editor
  scr_xsize = xsize*(1-self.plot_xfrac) > self.control_size[0]
  scr_ysize = ysize - self.control_size[1]
  widget_control, self.parinfo_baseID, $
                  scr_xsize=scr_xsize, $
                  scr_ysize=scr_ysize

  return, retval

end

;; Cleanup method
pro pfo_fit_obj::cleanup
  ;; Call our inherited cleaup routines
  self->pfo_cw_obj::cleanup
  ;; Nothing local to do
end

;; Init method
function pfo_fit_obj::init, $
   p0, $ ;; either the name of the object to create or one of the data vectors
   p1, $ ;; one of the data vectors ([x], y, [yerr])
   p2, $
   p3, $
   pfo_obj=pfo_obj, $ ;; optional input or output
   returning_pfo_obj=returning_pfo_obj, $ ;; flag to indicate pfo_fit caller wants pfo_obj that is initialized here returned
   title=title, $	;; title string
   menus=menus_in, $	;; list of menus to be used with pfo_menubar (see code for default list)
   additional_menus=additional_menus, $ ;; list of menus to be added to default pfo_fit list of menus (allows pfo_fit to improve without need to change calling code)
   menu_args=menu_args_in, $ ;; optional keyword argument(s) to the *_menu widget functions.  The current plotwin_obj is always added
   xsize=xsize, $ ;; x size of widget
   ysize=ysize, $ ;; y size of widget
   enable_undo=enable_undo, $ ;; by default enable_undo=1 for GUI.  Set enable_undo=0 to disable undo and save memory
   _REF_EXTRA=extra

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

  ;; Create pfo_obj, if necessary.
  if NOT obj_valid(pfo_obj) then begin
     ;; Create our pfo_obj
     pfo_obj = pfo_obj_new(p0, p1, p2, p3, _EXTRA=extra)
     ;; Created_pfo flag ensures pfo_obj is deleted.  If we want
     ;; pfo_obj as an output, don't delete it
     self.created_pfo_obj = ~returning_pfo_obj
  endif ;; creating pfo_obj

  ;; Default title
  if N_elements(title) eq 0 then $
     title = 'PFO_FIT'

  ;; Find a comfortable size for our widget
  ;; Get screen size
  device, get_screen_size=screen

  if N_elements(xsize) eq 0 then $
     xsize = 1500 < (screen[0] * 0.8)
  if N_elements(ysize) eq 0 then $
     ysize = 512 < (screen[1] * 0.8)
  self.plot_xfrac = .42

  ;; We need to syncronize the xsize and ysize of the pfo_plot_obj
  ;; property in the pfo_obj (or wherever the pfo_plot_obj ends up
  ;; residing) with the xsize and ysize of our draw widget.  Kind of
  ;; inconvenient to have to do it that way, but that is the
  ;; breaks....
  plot_xsize = xsize*self.plot_xfrac
  plot_ysize = ysize

  ;; enable undo in the GUI by default
  if N_elements(enable_undo) eq 0 then $
     enable_undo = 1
  
  ;; Set up our help string.
  help = title + ' help.  Data, function, and deviates are displayed in the left panel.  The top right displays the cursor location on the plot and allows adjustment of the plot limits (min and max).  The plot -> autoscale menu items reset user-specified plot limits.  The function editor is displayed in the lower righthand panel.  '
  help += string(10B) + string(10B)

  help += 'The FUNCTION EDITOR shows a set of equations that define the function to be fit to the data.  The basic equation always starts X = Xin; Y = NaN (or 0).  Subsequent sub-functions (e.g. pfo_poly) are combined using simple operations.  +, *, and "replacement" are currently supported and debugged.  Convolution is in the code but not debugged.  "Replacement" is an important operation, since it means that the Y-axis NaNs will be replaced by the value of the function. pfo_poly (usually used as a background) defaults to replacing the Y-axis for this reason.  Subsequent functions operate as desired by selecting the appropriate axis and operation.  Sophisticated algebra is not supported.  The order in which functions are listed is the order in which operations are performed.'
  help += string(10B) + string(10B)

  help += 'PARAMETER EDITING, VALUES and CONSTRAINTS.  When you "Add a new function" or click the "edit" button to the left of an existing function, a new window will appear with editable fields.  It is important to recognize that there are *four columns* of values that apply to the parameter: left limit, value, error, and right limit.  Note that the actual value of the parameter is the *SECOND* column of numbers.  This is the one you will usually want to edit first.  The reason for this is that MPFIT, the parameter optimizer used by PFO, allows left and right simple bounds to be placed on the excursion of a parameter during a fit.  The display of these left and right limits is to the left and right of the parameter value.  In order to enable the limit feature, use the pulldown menu in the "L" columns and select "<".  When you fit (fit menu), pay careful attention to whether or not your parameter becomes "pegged" at a limit value (indicated by "<*").  If this happens, you may want to fix it at that value (set the "L" column to "|") so that the number of fitted parameters comes out right.  An "L" value of "." means the parameter is free.'
  help += string(10B) + string(10B)

  ;; --> It might be nice to figure out how to add on help for parinfo
  ;; modules here.

  ;; Call our inherited init routines.  This creates a top-level base
  ;; with a menu bar and makes sure that the resize event can be
  ;; handled.  It also puts pfo_obj into self or creates it if none
  ;; was specified
  ok = self->pfo_cw_obj::init( $
       pfo_obj=pfo_obj, $
       title=title, $
       /mbar, $ ;; Give ourselves a menu bar
       /first_child, $ ;; hide cw_obj in a first child uvalue instead of tlb uvalue
       /tlb_size_events, $ ;; have IDL generate resize events
       uvalue={method: 'resize', obj:self}, $ ;; catch resize events (/first_child needed to let this work)
       help=help, $
       realize=0, $
       _EXTRA=extra)
  if NOT ok then begin
     message, 'WARNING: pfo_cw_obj::init returned error.  Object not properly initilized'
     return, 0
  endif

  ;; Put the customization derived above into the (possibly newly
  ;; created) pfo_obj
  self.pfo_obj->set_property, $
     plot_xsize=plot_xsize, $
     plot_ysize=plot_ysize, $
     enable_undo=enable_undo

  ;; Create a container into which we will put our plot window and
  ;; pfo_parinfo_container_cw
  self->create_container, column=2

  ;; Put our plot window in the first column.  Make sure the draw
  ;; widget is the same size as the plot object thinks
  ID = pfo_plotwin( $
       self.containerID, $
       pfo_obj=self.pfo_obj, $
       xsize=xsize*self.plot_xfrac, $
       ysize=ysize, $
       /keyboard_events, group_leader=self.tlbID, $ ;; --> have ctrl-d exit the widget for quick debugging
       cw_obj=cw_obj)
  self.plotwin_obj = cw_obj

  ;; Make a container for the second column
  col2ID = widget_base(self.containerID, /column)
  ;; Make a container for things like the cursor and plotting controls
  controlID = widget_base(col2ID, /column)
  ;; Make the column heading widgets
  ID = pfo_cursor_colhead_cw(controlID, label_width=0.5, pfo_obj=self.pfo_obj, plotwin_obj=self.plotwin_obj)
  ;; Cursor
  ID = pfo_cursor_cw(controlID, label_width=0.5, pfo_obj=pfo_obj, plotwin_obj=self.plotwin_obj)
  ;; Range control
  ID = pfo_range_cw(controlID, label_width=0.5, pfo_obj=pfo_obj, plotwin_obj=self.plotwin_obj, /no_colheads)

  ;; Figure out how big the control section is so we can size the
  ;; parinfo editor accordingly
  geom = widget_info(controlID, /geometry)
  self.control_size = [geom.xsize, geom.ysize]

  ;; Create a base with scrollbars in the bottom part of the second
  ;; column into which we will put our parinfo editor
  x_scroll_size = xsize*(1-self.plot_xfrac) > self.control_size[0]
  y_scroll_size = ysize - self.control_size[1]
  self.parinfo_baseID = $
     widget_base(col2ID, $
                 x_scroll_size=x_scroll_size, $
                 y_scroll_size=y_scroll_size)

  ;; Since some of our menus relate to the compound widgets we are
  ;; displaying, set menus up last
  
  ;; Set up the drop-down menus.  Allow entire list to be overridden 
  if N_elements(menus_in) ne 0 then $
     menus = menus_in
  if N_elements(menus) eq 0 then $
     menus = ['pfo_generic', 'pfo_plot', 'pfo_parinfo_edit', 'pfo_fit']
  ;; Allow menu items to be added without altering the default list.
  ;; Quietly does nothing if additional_menus is undefined
  pfo_array_append, menus, additional_menus, /quiet

  ;; Protect input menu_args list
  if N_elements(menu_args_in) ne 0 then $
     menu_args = menu_args_in
  ;; Put our plotwin_obj on the menu_args list
  pfo_array_append, menu_args, {plotwin_obj:self.plotwin_obj}

  ;; Insert the menu bar menus into the menu bar.  Help is always the
  ;; last menu
  self->mbar, menus, menu_args=menu_args

  ;; Put our parinfo display widget in the second column
  self.pfo_obj->parinfo_edit, status_mask=!pfo.all_status, $
                              parentID=self.parinfo_baseID

  return, 1
end

;; Widget controlling object class definition
pro pfo_fit_obj__define
  objectClass = $
     {pfo_fit_obj, $
      plot_xfrac	: 0., $ ;; fraction of total widget X-dimension reserved for plot
      control_size	: [0., 0.], $ ;; size of base containing cursor/plot controls
      plotwin_obj	: obj_new(), $ ;; cw_obj of plotwin
      parinfo_baseID	: 0L, $ ;; widgetID of scroll bar-enabled base that contains parinfo editor
      inherits pfo_cw_obj}
end

pro pfo_fit, $
   p0, $ ;; either the name of the object to create or one of the data vectors
   p1, $ ;; one of the data vectors ([x], y, [yerr])
   p2, $
   p3, $
   pfo_obj=pfo_obj, $ ;; optional input or output
   $ ;; By default, widget is realized, but you may which to avoid that for some reason
   mbarID=mbarID, $ ;; (output) ID of the menubar widget
   containerID=containerID, $ ;; (output) optional parent widget of any subsequent children of this base
   cw_obj=cw_obj, $ ;; (output) the object that runs this cw
   realize=realize_in, $ ;;  be polite to calling code
   $ ;; By default, the widget is non-blocking (other events and the IDL command line are processed).
   $ ;; You may wish to set no_block=0 to force user to finish with the widget before other things happen.  
   no_block=no_block_in, $ ;;  be polite to calling code
   _REF_EXTRA=extra

  ;; Make sure our system variables are defined for all of the
  ;; routines in this file
  init = {tok_sysvar}
  init = {pfo_sysvar}

  ;; Handle pfo_debug level.  CATCH errors if _not_ debugging
  if !pfo.debug le 0 then begin
     ;; Return to the calling routine with our error
     ON_ERROR, !tok.return
     CATCH, err
     if err ne 0 then begin
        CATCH, /CANCEL
        message, /NONAME, !error_state.msg, /CONTINUE
        message, /CONTINUE, 'ERROR: caught the above error.  Returning with what I have so far.'
        return
     endif
  endif ;; not debugging

  ;; Create our widget controlling object.
  cw_obj = obj_new('pfo_fit_obj', p0, p1, p2, p3, $
                   pfo_obj=pfo_obj, $
                   returning_pfo_obj=arg_present(pfo_obj), $
                   _EXTRA=extra)

  if obj_valid(cw_obj) then begin
     ;; Successful cw_obj creation
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
     ;; object.  Handle realize in a polite way to the caller
     if N_elements(realize_in) ne 0 then $
        realize = realize_in
     if N_elements(realize) eq 0 then $
        realize = 1
     if keyword_set(realize) then begin
        ;; Realize the widget
        widget_control, realize=realize, cwID
        ;; Issue the replot method (which would realize the
        ;; widget_draw window anyway) now that we have a window in
        ;; which to do the plot
        pfo_obj->replot
        ;; Issue the refresh method
        pfo_obj->refresh
     endif ;; realizing

     ;; Handle no_block in a polite way to the caller, since we may
     ;; change it
     if N_elements(no_block_in) ne 0 then $
        no_block = no_block_in
     ;; Default is to raise a non-blocking widget
     if N_elements(no_block) eq 0 then $
        no_block = 1
     ;; This is a true tlb, so we need to call xmanager and pass it an
     ;; event handler that is a procedure.  The only events it will
     ;; handle are tlb resize events.  But never fear, it does it with
     ;; the cw_obj object-oriented event handling system.
     xmanager, 'pfo_fit', cwID, event_handler='pfo_cw_event_pro', no_block=no_block

  endif ;; valid cw_obj

end

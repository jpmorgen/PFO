;+
; NAME: pfo_null__widget
;
; PURPOSE: Provide generic widget display services for simple PFO functions
;
; CATEGORY: PFO functions
;
; CALLING SEQUENCE: this is called from within pfo_parinfo_parse

; DESCRIPTION: This creates a compound widget which displays the
; parinfo of one function.  It is intended to be called from within
; pfo_parinfo_parse, which itself was called within the pfo_obj so
; that the parinfo and pfo_obj's *self.pparinfo are the same physical
; space in memory (e.g. with the pfo_parinfo_obj::widget method).  The
; widgets of the individual parameters are registered with the refresh
; list in the pfo_obj so that any changes made anywere in the system
; can be kept current (as long as the routine doing the changing does
; a pfo_obj->refresh).  

; This widget is not designed to independently decide what portion of
; the parinfo it should display, so it does not create a repopulatable
; pfo_cw.  Work at the next level up (e.g., where you choose the idx)
; to make repopulatable widgets.

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
; $Id: pfo_null__widget.pro,v 1.6 2012/01/13 20:58:04 jpmorgen Exp $
;
; $Log: pfo_null__widget.pro,v $
; Revision 1.6  2012/01/13 20:58:04  jpmorgen
; Add tied widget
;
; Revision 1.5  2011/11/18 15:34:56  jpmorgen
; Changed to use pfo_obj print method
;
; Revision 1.4  2011/09/23 13:06:22  jpmorgen
; Minor change
;
; Revision 1.3  2011/09/16 11:22:51  jpmorgen
; Significant improvement in widget speed by using pfo_parinfo_text_cw
; and edit button in pfo_parinfo_mode_cw
;
; Revision 1.2  2011/09/08 20:01:17  jpmorgen
; Played with /align_right and found it not necessary.  It seems to mess
; with widget_label in pfo_parinfo_error_xw
;
; Revision 1.1  2011/09/01 22:11:11  jpmorgen
; Initial revision
;
;-

;; This dumps our widgets into an already defined self.containerID,
;; established when we called pfo_cw_obj::init
pro pfo_null_cw_obj::populate, $
   params=params, $ 	;; parameters (entire array).
   fname=fname, $ ;; original fname of function in pfo_parinfo_parse
   first_funct=first_funct, $ ;; allows us to display some introductory material (e.g. column headings)
   edit=edit, $ ;; bring up all widgets rather than just text widget with parinfo printed into it
   no_mode=no_mode, $ ;; don't put the mode buttons on the widget (useful when editing a new parinfo segment)
   no_finit=no_finit, $ ;; Don't display X=Xin, Y=NaN function initialization string (e.g. you are displaying a segment of parinfo)
   param_names_only=param_names_only, $ ;; print parameter names only
   brief=brief, $ ;; print the function briefly on one line (just function name and parameters)
   full=full, $ ;; print function name and algebraic info on a line by itself and then one line per parameter
   include_mpstep=include_mpstep, $, ;; also include MPFIT autoderivative step stuff (usually defaults are fine, so these are just zero)
   _REF_EXTRA=extra ;; Extra keywords to pass on to *struct__widget methods

  ;; Handle pfo_debug level.  CATCH errors if _not_ debugging
  if !pfo.debug le 0 then begin
     CATCH, err
     if err ne 0 then begin
        CATCH, /CANCEL
        junk = widget_text(self.tlbID, value='Caught the following error: ' + !error_state.msg)
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'ERROR: caught the above error.  Returning widget as prepared so far,', /CONTINUE
        return
     endif
  endif ;; not debugging

  ;; Do basic function consistency checking
  self.pfo_obj->parinfo_call_procedure, $
     /no_update, 'pfo_fcheck', fname, params=params, idx=*self.pidx, pfo_obj=self.pfo_obj

  ;; Work out our default layout.
  if keyword_set(param_names_only) + keyword_set(brief) + keyword_set(full) eq 0 then $
     full = 1
  if keyword_set(param_names_only) + keyword_set(brief) + keyword_set(full) ne 1 then $
     message, 'ERROR: specify only one keyword: param_names_only, brief, or full.  Default is brief.'
  
  ;; Define our column widths.
  units = !tok.inches
  mode_width = 1.5
  if keyword_set(no_mode) then $
     mode_width = 0
  parname_width = 0.75
  val_width = 1.
  err_width = 0.75
  delim_width = 0.65
  pm_width = 0.25

  ;; See if we need to display our preamble
  if keyword_set(first_funct) then begin
     ;; Allow user to skip finit (X=Xin, Y=NaN) if they are not
     ;; displaying a whole function.
     if NOT keyword_set(no_finit) then $
        ID = pfo_finit_cw(self.tlbID, pfo_obj=self.pfo_obj)

     ;; Create a row widget into which to put our delimiter key
     rowID = widget_base(self.tlbID, /row, /base_align_center)
     ;; Remember to move it over by mode_width
     ID = widget_base(rowID, xsize=mode_width, units=units)
     ID = widget_label(rowID, value='(L: . = free, | = fixed, < = limited, <* = pegged)')

     ;; If we are editing, do this in widget-land
     if keyword_set(edit) then begin
        ;; Create a row widget into which to put our column headings
        rowID = widget_base(self.tlbID, /row, /base_align_center, /frame)
        ;; Remember to move it over by mode_width, if necessary
        ID = widget_base(rowID, xsize=mode_width, units=units)
        ID = widget_label(rowID, value='Param name', xsize=parname_width, units=units)
        ID = widget_label(rowID, value='Left limit', xsize=val_width, units=units)
        ID = widget_label(rowID, value=' L  ', xsize=delim_width, units=units)
        ID = widget_label(rowID, value='Value', xsize=val_width, units=units)
        ID = widget_label(rowID, value='', xsize=pm_width, units=units)
        ID = widget_label(rowID, value='Error', xsize=err_width, units=units)
        ID = widget_label(rowID, value='L   ', xsize=delim_width, units=units)
        ID = widget_label(rowID, value='Right limit', xsize=val_width, units=units)
        if keyword_set(include_mpstep) then begin
           mpside_width = delim_width*3
           ID = widget_label(rowID, value='Step', xsize=val_width, units=units)
           ID = widget_label(rowID, value='Rel Step', xsize=val_width, units=units)
           ID = widget_label(rowID, value='MPside', xsize=mpside_width, units=units)
           ID = widget_label(rowID, value='MPmaxstep', xsize=val_width, units=units)
        endif ;; optional MPside stuff
        ID = widget_label(rowID, value='idx', xsize=delim_width, units=units)        
        ID = widget_label(rowID, value='tied', xsize=val_width, units=units)        
        ;; Call the col_head sections of any other widget routines in
        ;; the parinfo structure.  Make use of the call procedure system
        ;; in the pfo_obj and then again at the struct level.
        junk = self.pfo_obj->parinfo_call_function( $
               /no_update, 'pfo_struct_call_function', 'widget', rowID, /col_head, $
               idx=*self.pidx, _EXTRA=extra)

     endif else begin
        ;; If we are printing into text widgets to go faster, our
        ;; formatting will be different

        ;; Create a row widget into which to put our column headings
        rowID = widget_base(self.tlbID, /row, /base_align_center, /frame)
        ;; Remember to move it over by mode_width
        ID = widget_base(rowID, xsize=mode_width, units=units)

        junk =  self.pfo_obj->print(/full, $
                col_head=col_head, idx=*self.pidx, _EXTRA=extra)

        ;; Select a fixed-width font we know is going to be there so
        ;; things line up with FORMAT statements. --> a better font
        ;; could be found -- what do they use for the terminal window
        ;; in IDL?
        if !d.name eq 'WIN' then $
           font = !pfo.win_font

        ;; Seems to need an extra space to get things to line up
        ID = widget_label(rowID, value=' '+col_head, font=font)


     endelse ;; printing into a text widget

  endif ;; Preamble stuff

  ;; Get current values for things
  self.pfo_obj->parinfo_call_procedure, $
     /no_update, 'pfo_struct_setget_tag', /get, idx=*self.pidx, $
     taglist_series='pfo', inaxis=inaxis, outaxis=outaxis, $
     fop=fop, ftype=ftype, infunct=infunct, outfunct=outfunct, $
     pfoID=pfoID, fseq=fseq

  ;; Check to make sure that we don't have fancy functions with
  ;; multiple axes/fops going on
  junk = uniq(inaxis, sort(inaxis))
  if N_elements(junk) ne 1 then begin
     message, 'ERROR: more than one inaxis specified for ' + pfo_fname(ftype[0], pfo_obj=self.pfo_obj) + ' idx = ' + pfo_array2str(idx) + '.  If this function can really work that way, write its own __widget routine to deal with each parameter individually.'
  endif
  junk = uniq(outaxis, sort(outaxis))
  if N_elements(junk) ne 1 then begin
     message, 'ERROR: more than one outaxis specified for ' + pfo_fname(ftype[0], pfo_obj=self.pfo_obj) + ' idx = ' + pfo_array2str(idx) + '.  If this function can really work that way, write its own __widget routine to deal with each parameter individually.'
  endif
  junk = uniq(fop, sort(fop))
  if N_elements(junk) ne 1 then begin
     message, 'ERROR: more than one fop specified for ' + pfo_fname(ftype[0], pfo_obj=self.pfo_obj) + ' idx = ' + pfo_array2str(idx) + '.  If this function can really work that way, write its own __widget routine to deal with each parameter individually.'
  endif
  junk = uniq(infunct, sort(infunct))
  if N_elements(junk) ne 1 then begin
     message, 'ERROR: more than one infunct specified for ' + pfo_fname(ftype[0], pfo_obj=self.pfo_obj) + ' idx = ' + pfo_array2str(idx) + '.  If this function can really work that way, write its own __widget routine to deal with each parameter individually.'
  endif
  junk = uniq(outfunct, sort(outfunct))
  if N_elements(junk) ne 1 then begin
     message, 'ERROR: more than one outfunct specified for ' + pfo_fname(ftype[0], pfo_obj=self.pfo_obj) + ' idx = ' + pfo_array2str(idx) + '.  If this function can really work that way, write its own __widget routine to deal with each parameter individually.'
  endif

  ;; Create a two-column base for each function.  Column one has the
  ;; active, inactive, delete and edit buttons and column two is a row
  ;; base that has the function.

  ;; --> In WINDOWS, the dotted frames around selected buttons in the
  ;; pfo_parinfo_mode_cw only show up when .r pfo_null__widget is
  ;; explicitly issued at the command line, at least in 7.1.1

  ;; Only put the mode buttons on if the user has not disabled them
  ;; (e.g. when editing a new parinfo)
  fbaseID = widget_base(self.tlbID, column=1 + (~keyword_set(no_mode)))
  if NOT keyword_set(no_mode) then $
     ID = pfo_parinfo_mode_cw(fbaseID, idx=*self.pidx, xsize=mode_width, units=units, $
                              edit=edit, pfo_obj=self.pfo_obj)

  ;; Only allow tabbing into base containing the function widgets if
  ;; we are in editing mode.  --> this didn't work in LINUX and the Windows
  col2ID = widget_base(fbaseID, /column, tab_mode=keyword_set(edit))

  ;; Check to see if we are in full-up edit mode, in which case we
  ;; display all of our widgets
  if keyword_set(edit) then begin
     ;; Create a row widget into which to put our equation.
     rowID = widget_base(col2ID, /row, /base_align_center)
     ;; Remember to pass all of the idx so the whole function can be
     ;; changed by the event functions

     ;; Outaxis
     ID = pfo_parinfo_axis_cw(rowID, /out, idx=*self.pidx, pfo_obj=self.pfo_obj)
     ;; =
     ID = widget_label(rowID, value='=')
     ;; Don't repeat the outaxis when we have operations of noop or repl.
     ;; When we do want to repeat the outaxis, just make it a label, so
     ;; it is clear we can't have y = x without going through a poly or
     ;; something like that --> make a widget for this, so it works with refesh
;;  if NOT (fop[0] eq !pfo.noop or fop[0] eq !pfo.repl) then $
     ;;ID = widget_label(rowID, value=!pfo.widget_axis_string[outaxis[0]])

     ;; Just use the droplist widget to always repeat the outaxis for now
     ID = pfo_parinfo_axis_cw(rowID, /out, idx=*self.pidx, pfo_obj=self.pfo_obj)

     ;; Operation.  Always list this so it can be changed
     ID = pfo_parinfo_fop_cw(rowID, idx=*self.pidx, pfo_obj=self.pfo_obj)
     
     ;; Outfunct: transformation performed to output (e.g. alog) --> make
     ;; a widget for parenthesis
     ID = pfo_parinfo_xfunct_cw(rowID, /out, idx=*self.pidx, pfo_obj=self.pfo_obj)
     ID = widget_label(rowID, value='(')

     ;; Function.  For now, we are not going to change functions, so
     ;; widget_label is OK.  Eventually a dropdown might be in order, but
     ;; that would need code to translate from one function to another.
     ID = widget_label(rowID, value=pfo_fname(ftype[0], pfo_obj=self.pfo_obj))
     ;; Function ID --> make a widget
     ID = fsc_field(rowID, title='', value=pfoID[0], decimal=0, digits=3, xsize=3, /NONSENSITIVE)

     ;; Inaxis stuff, including ()
     ID = widget_label(rowID, value='(')
     ;; Infunct: transformation performed on input axis (e.g. exp) -->
     ;; make a widget for parenthesis
     ID = pfo_parinfo_xfunct_cw(rowID, /in, idx=*self.pidx, pfo_obj=self.pfo_obj)
     ID = widget_label(rowID, value='(')
     ;; Inaxis
     ID = pfo_parinfo_axis_cw(rowID, /in, idx=*self.pidx, pfo_obj=self.pfo_obj)
     ID = widget_label(rowID, value=')')
     ;; Close paranthesis from infunct
     ID = widget_label(rowID, value=')')
     ;; Close paranthesis from outfunct
     ID = widget_label(rowID, value=')')

     ;; fseq --> make a real widget
     ID = widget_label(rowID, value='fseq = ')
     ID = fsc_field(rowID, title='', value=fseq[0], decimal=0, digits=3, xsize=3, /NONSENSITIVE)

     ;; Check to see if any of the parinfo structure tags want to
     ;; display any other widgets on the equation line
     ;; (e.g. pfo_ROI_struct).  This actually returns an array of
     ;; structures, where the ID.result is the array of widget IDs
     ID = self.pfo_obj->parinfo_call_function( $
          /no_update, 'pfo_struct_call_function', 'widget', /equation, rowID, idx=*self.pidx, _EXTRA=extra)

     ;; PARAMETERS
     for ip=0, N_elements(*self.pidx)-1 do begin

        ;; Read our value from the parinfo.  Also read whether or not we are
        ;; tied so that we can make ourselved insensitive
        self.pfo_obj->parinfo_call_procedure, $
           /no_update, 'pfo_struct_setget_tag', /get, idx=(*self.pidx)[ip], $
           taglist_series='mpfit_parinfo', parname=parname

        ;; Create a row widget into which to put our parameters
        rowID = widget_base(col2ID, /row, /base_align_center)
        ;; Parameter name.  widget_label width is in characters only
        ID = widget_label(rowID, value=parname, xsize=parname_width, units=units)
        ;; Left limit
        ID = pfo_parinfo_limit_cw(rowID, side=!pfo.left, idx=(*self.pidx)[ip], pfo_obj=self.pfo_obj, $
                                  xsize=val_width, units=units)
        ;; Left deliminter
        ID = pfo_parinfo_delimiter_cw(rowID, side=!pfo.left, idx=(*self.pidx)[ip], pfo_obj=self.pfo_obj, $
                                      xsize=delim_width, units=units)
        ;; Value
        ID = pfo_parinfo_value_cw(rowID, idx=(*self.pidx)[ip], pfo_obj=self.pfo_obj, $
                                  xsize=val_width, units=units)
        ;; +/-
        ID = widget_label(rowID, value='+/-', xsize=pm_width, units=units)
        ;; Error
        ID = pfo_parinfo_error_cw(rowID, idx=(*self.pidx)[ip], pfo_obj=self.pfo_obj, $
                                  xsize=err_width, units=units)
        ;; Right delimiter
        ID = pfo_parinfo_delimiter_cw(rowID, side=!pfo.right, idx=(*self.pidx)[ip], pfo_obj=self.pfo_obj, $
                                      xsize=delim_width, units=units)
        ;; Right limit
        ID = pfo_parinfo_limit_cw(rowID, side=!pfo.right, idx=(*self.pidx)[ip], pfo_obj=self.pfo_obj, $
                                  xsize=val_width, units=units)
        ;; idx.  If this changes, we will be repopulating
        ID = widget_label(rowID, value=string(format='(i4)', (*self.pidx)[ip]), xsize=pm_width, units=units)
        ;; tied.  Make it small for now, since all it will typically
        ;; display is P[xxx]
        ID = pfo_parinfo_tied_cw(rowID, idx=(*self.pidx)[ip], pfo_obj=self.pfo_obj, $
                                 xsize=val_width, units=units)

        ;; Call the other __widget functions in the parinfo structure
        ;; *on each parameter*
        ID = self.pfo_obj->parinfo_call_function( $
             /no_update, 'pfo_struct_call_function', 'widget', /parameter, rowID, idx=(*self.pidx)[ip], _EXTRA=extra)

     end ;; each parameter
  endif else begin
     ;; We are just printing information into a text widget to save
     ;; time
     ID = pfo_parinfo_text_cw(col2ID, idx=*self.pidx, pfo_obj=self.pfo_obj)
  endelse 
end

;; Cleanup method
pro pfo_null_cw_obj::cleanup
  ;; Call our inherited cleaup routines
  self->pfo_parinfo_cw_obj::cleanup
end


;; Init method
function pfo_null_cw_obj::init, $
   parentID, $ ;; widgetID of parent widget
   idx=idx,  $ ;; make sure we capture idx here, so it doesn't get propagated through to the _EXTRA in the populate
   _REF_EXTRA=extra ;; All other input parameters passed to other initilization routines via _EXTRA mechanism

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

  ;; Call our inherited init routines.  This puts pfo_obj and idx of
  ;; this function into self, among other things.  It also makes the
  ;; tlb a column base.
  ok = self->pfo_parinfo_cw_obj::init(parentID, idx=idx, /column, _EXTRA=extra)
  if NOT ok then begin
     message, 'WARNING: object not initialized properly'
     return, 0
  endif

  ;; Build our widget
  self->populate, _EXTRA=extra

  ;; If we made it here, we have successfully set up our container.  
  return, 1

end

;; Object class definition.  --> For now, this could be plain pfo_cw,
;; since it doesn't make use of any refresh or repopulate
;; capabilities, but it might some day do an internal repopulate if I
;; add the capability to reconfigure the widget on the fly
;; (e.g. switch from full to brief).  In either case, having the idx
;; encapsulated, which comes with being a pfo_parinfo_cw_obj is handy
pro pfo_null_cw_obj__define
  objectClass = $
     {pfo_null_cw_obj, $
      inherits pfo_parinfo_cw_obj}
end


;;-------------------------------------------------------------------
;; MAIN ROUTINE
;;-------------------------------------------------------------------

;; NOTE on parinfo keyword.  The parinfo encapsulated in pfo_obj is
;; used everywhere in the pfo_parinfo_cw_obj system.  In fact, to
;; specify it raises an error in the init of pfo_parinfo_cw_obj.
;; pfo_parinfo_parse, which calls this routine, passes it parinfo.  So
;; we need to make sure parinfo is not hanging around in _EXTRA to be
;; passed on to our init method.  We just let it be specified and
;; ignore it.
function pfo_null__widget, $
   parentID, $ ;; Parent widget ID (positional parameter)
   cw_obj=cw_obj, $ ;; (output) the object that runs this cw
   _REF_EXTRA=extra ;; All other input parameters, including pfo_obj, 
  ;; are passed to the init method and underlying routines via _REF_EXTRA mechanism

  ;; Generic pfo system initialization
  init = {pfo_sysvar}
  init = {tok_sysvar}

  ;; Initialize our output
  cwID = !tok.nowhere

  ;; Create our controlling object
  cw_obj = obj_new('pfo_null_cw_obj', parentID, _EXTRA=extra)

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

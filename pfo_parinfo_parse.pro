;; -*- mode: idlwave; fill-column: 125 -*-
;+
; NAME: pfo_parinfo_parse
;
; PURPOSE: interpret functions (parinfo arrays) in the PFO system
;
; CATEGORY: PFO
;
; CALLING SEQUENCE:  One of the following, depending on the desired action:

; yaxis = pfo_parinfo_parse(/CALC, parinfo=parinfo [, params=params] Xin=Xin[, idx=idx][, ispec=ispec][, iROI=iROI][, allspec=allspec][, allROI=allROI][, pfo_obj=pfo_obj][, xaxis=xaxis][, ROI_Xin_idx=ROI_Xin_idx][, keyowrds to pfo_<fname> routines])

; toprint = pfo_parinfo_parse(/PRINT, parinfo=parinfo[, params=params][, idx=idx][, ispec=ispec][, iROI=iROI][, allspec=allspec][, allROI=allROI][, status_mask=status_mask][, pfo_obj=pfo_obj][, keyowrds to pfo_<fname> routines])

; widget_id = pfo_parinfo_parse(/WIDGET, parinfo=parinfo[, params=params][, idx=idx][, ispec=ispec][, iROI=iROI][, allspec=allspec][, allROI=allROI][, pfo_obj=pfo_obj][, status_mask=status_mask][, keyowrds to pfo_<fname>__widget routines])

; indices = pfo_parinfo_parse(/INDICES, parinfo=parinfo[, params=params][, idx=idx][, expand_idx=expand_idx][, /terminate_idx][, ispec=ispec][, iROI=iROI][, allspec=allspec][, allROI=allROI][, status_mask=status_mask][, pfo_obj=pfo_obj][, keyowrds to pfo_<fname>__indices routines])

; DESCRIPTION:

; pfo_parinfo_parse parses a parinfo array according to the following
; hierarchy:

;   ispec, iROI, fseq, inaxis, outaxis, fop, ftype, ID (old)
;   ispec, iROI, fseq, outaxis, fop, inaxis, ftype, ID

; Each of these are sorted in numeric order before they are
; processed.  The numeric order of the axis and fop tokens can be
; found in pfo_sysvar__define.pro

; The final level is an individual instance of a PFO-enabled function.
; The appropriate "method" of this function is called ("__CALC,"
; "__PRINT," "__WIDGET," or "__INDICES") depending on which action
; switch was specified to pfo_parinfo_parse.  If no such method is
; found for the PFO-enabled function, the appropriate method from
; pfo_null is called.  For instance, the methods pfo_null__print and
; pfo_null__widget should take care of display of parameters from most
; simple functions.

; The return value of the PFO-enabled function is combined with the
; previous function return values in a manner specified by tags in
; parinfo.pfo (/CALC case) by keywords (/INDICES case).  In the /PRINT
; case, all output is collected into one string.  For /WIDGET, one
; widgetID is returned and information which enables higher-level
; routines to refresh (update individual widgets) or repopulate
; (completely recreate the widget) is stored in pfo_obj.

; INPUTS:

;  parinfo: array of type structure  which describes the function
;
; OPTIONAL INPUTS:

;   params: parameter values in place of parinfo.value.  Usually
;	Provided by parameter optimization routine.


; KEYWORD PARAMETERS:

;   idx: indices into parinfo over which operation will be performed.
;   	If not specified, the entire parinfo array will be used
;   	(subject to ispec and iROI selections)

;   ispec: the spectrum number(s) (second dimension of Xin) which will
;   be selected from within the parinfo selected by idx (see pfo_ROI_idx)

;   iROI: the ROI number(s) (sections along Xin) which will
;   be selected in each ispec (see pfo_ROI_idx)

;   allspec: a special code is assigned to functions that are
;   intended to operate on all spectra.  When selecting a particular
;   ispec, it is easy to forget to include functions that are intended
;   to operate on all spectra, so this keyword is provided as a
;   reminder.  Using it is equivalent to ispec=[!pfo.allspec, ispec]

;   allROI: same as allspec, but includes the !pfo.allROI in iROI.
;   The iROI = !pfo.allROI is a special case which lets us have
;   multiple ROI functions across the X-axis _with the same iROI_.
;   The advantage is that we can can assign functions to
;   iROI=!pfo.allROI and have them over all of those ROIs at once.
;   Otherwise, we would need to define the function on each ROI and
;   link the parameters together.

;   status_mask: passed on to pfo_fidx.  By default, only active
;   parameters are handled.  status_mask allows parameters marked
;   !pfo.inactive and !pfo.delete to be used in the function too.
;   Essential for printing and widget display.

;   pfo_obj: pfo_obj which contains parinfo array and other
;		information necessary for the PFO system

;   Additional keywords for /CALC

;       Xin: X-axis intended to be in natural units of a detector
;       (e.g. CCD pixels or MCA channels)

;       xaxis: internal xaxis.  In pfo_parinfo_parse, Xin, the input X-axis is
;       assumed to be in natural detector units.  It can be
;       transformed by operations with parinfo.pfo.inaxis = !pfo.Xin
;       and parinfo.pfo.outaxis = !pfo.xaxis into an internal xaxis
;       which itself can then be used in calculations to output to the
;       yaxis.  This keyword provides axis to the internal xaxis.

;	ROI_Xin_idx (output): indices into Xin selected by ROI function(s)

;   Additional keywords for /INDICES

;       expand_idx: this allows the index of a single parameter into
;       parinfo to return all of the indices into the function in
;       which it is contained.  Return value of pfo_parinfo_parse is
;       expanded index list.  Multiple functions can be expanded if
;       expand_idx is a vector.  If more expand_idx contains more than
;       one index into a particular function, the indices into that
;       function will only be included once in the returned index
;       list.  Example:

;	;; Return indices of just the peak parameter(s)
;	junk = pfo_parinfo_parse(/INDICES, parinfo=parinfo, peak_idx=peak_idx)
;	;; Expand peak parameter indices into indices for the entire function(s)
;	peak_fn_idx = pfo_parinfo_parse(/INDICES, parinfo=parinfo, expand_idx=peak_idx)

;       terminate_idx: put termination marks (!tok.nowhere) after each
;       set of indices in all returned index lists.  These can be
;       parsed with pfo_idx_parse

;   Additional keywords for /WIDGET

;       parentID (optional): the parentID of a widget in which this
;       compound widget will be displayed.  If not specified, a
;       free-floating widget will be created

; OUTPUTS:

;   CALC: returns calucated value of function given the Xin input axis

;   PRINT: returns a string that displays the function in parinfo.
;   Keywords can be used to change verbosity

;   WIDGET: returns the widgetID of a compound widget that allows
;   interactive modification and dynamic updates of information stored
;   in parinfo.  Keywords and subsequent widget events affect
;   look-and-feel of widget

;   INDICES: returns list of indices into parinfo after parsing has
;   been done.  Subset of indices is returned depending on input
;   keywords.  Indices corresponding to particular parameters in
;   individual functions can be returned by specifying the
;   corresponding keywords (see __INDICES "methods" in individual PFO
;   fdefine files).


; OPTIONAL OUTPUTS:
;
; COMMON BLOCKS:  
;   See pfo_finfo.
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
; $Id: pfo_parinfo_parse.pro,v 1.11 2012/04/14 13:27:56 jpmorgen Exp $
;
; $Log: pfo_parinfo_parse.pro,v $
; Revision 1.11  2012/04/14 13:27:56  jpmorgen
; Fixed reverse index problem on Mac
;
; Revision 1.10  2011/11/21 15:29:30  jpmorgen
; Fix bug with too many terminate_idxs
;
; Revision 1.9  2011/11/18 15:31:54  jpmorgen
; Change call to use parinfo as a keyword rather than a positional
; parameter so that upstream use of _REF_EXTRA in pfo_calc_obj results
; in smooth override of parinfo and all other keyword parameters.
;
; Revision 1.8  2011/09/23 01:59:57  jpmorgen
; Changed to use pfo_parinfo_edit and minor things
;
; Revision 1.7  2011/09/16 11:20:32  jpmorgen
; Propagated status_mask to primitives, since they (particularly
; __indices) call pfo_fidx.
;
; Revision 1.6  2011/09/15 20:53:32  jpmorgen
; Changed widget system to use pfo_parinfo_edit
;
; Revision 1.5  2011/09/08 20:07:41  jpmorgen
; Fix bug with no initial parinfo in widget (work in progress)
;
; Revision 1.4  2011/09/01 22:16:44  jpmorgen
; Significant improvements to parinfo editing widget, created plotwin
; widget, added pfo_poly function.
;
; Revision 1.3  2011/08/12 12:06:25  jpmorgen
; About to change order of parsing
;
; Revision 1.2  2011/08/02 18:16:57  jpmorgen
; Release to Tom
; Updated for !pfo.Yaxis_init, better handling of no parinfo input
;
; Revision 1.1  2011/08/01 19:18:16  jpmorgen
; Initial revision
;
;-

;; We need to make sure pfo_ROI__calc doesn't get interpreted as an array if we haven't compiled the file pfo_roi_fdefine.pro yet
forward_function pfo_ROI__calc

;; This is a helper function that keeps the code below from getting cluttered when dealing with errors at each nested loop level
function pfo_parinfo_parse_catch, action, toprint, indices_idx, yaxis, error, containerID

  error = 1

  ;; Prepare terminal message and retval
  terminal_msg = 'WARNING: caught the above error.  '
  print_msg = '% PFO_PARINFO_PARSE: ERROR: Function did not parse properly.  See terminal window.'
  case action of
     !pfo.print : begin
        terminal_msg += 'Returning what I have so far.'
        retval = toprint + !tok.newline + print_msg
     end
     !pfo.widget : begin
        terminal_msg += 'Returning an invalid widgetID.'
        retval = !tok.nowhere
     end
     !pfo.indices : begin
        if N_elements(indices_idx) eq 0 then begin
           terminal_msg += 'Returning -1'
           retval = !tok.nowhere
        endif else begin
           terminal_msg += 'Returning what I have so far.'
           retval = indices_idx
        endelse
     end
     !pfo.calc : begin
        terminal_msg += 'Returning what I have so far.'
        retval = yaxis
     end
  endcase ;; different error return values for different actions

  ;; Print terminal message
  message, /NONAME, !error_state.msg, /CONTINUE
  message, /CONTINUE, terminal_msg + '  You can use pfo_debug to help track down the problem.'

  ;; Do additional display, if necessary
  case action of
     !pfo.widget : begin
        if N_elements(containerID) ne 0 then $
           if widget_info(containerID, /valid_ID) then $
              ID = widget_label(containerID, value=print_msg)
        junk = dialog_message('% PFO_PARINFO_PARSE: ERROR: Function did not parse properly.  See terminal window for details.')
     end
     else :
  endcase ;; different additional display for different actions

  return, retval

end

function pfo_parinfo_parse, $
   parinfo=parinfo, params=params_in, idx=idx, ispec=ispec, iROI=iROI, pfo_obj=pfo_obj, $
   status_mask=status_mask, error=error, $
   calc=calc, Xin=Xin, xaxis=xaxis, ROI_Xin_idx=ROI_Xin_idx, convol_center=convol_center, $
   print=print, $
   indices=indices, expand_idx=expand_idx, terminate_idx=terminate_idx, $
   widget=widget, parentID=parentID_in, group_leader=group_leader, containerID=containerID, cw_objs=cw_objs, $
   _REF_EXTRA=extra

  init = {tok_sysvar}
  init = {pfo_sysvar}

  ;; Handle pfo_debug level.  CATCH invocation errors if _not_ debugging
  if !pfo.debug le 0 then begin
     ;; Return to the calling routine with our error
     ON_ERROR, !tok.return
     CATCH, err
     if err ne 0 then begin
        CATCH, /CANCEL
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'USAGE: return_value = pfo_parinfo_parse(/CALC | /PRINT | /WIDGET | /INDICES, parinfo=parinfo [, params=params][, Xin=Xin][, idx=idx]][, pfo_obj=pfo_obj][, xaxis=xaxis][, keyowrds to pfo_<fname> routines])'
     endif
  endif ;; not debugging

  ;; Do some setup work

  ;; Set up our different actions for the meat of the code.  Doing it this way allows us to use case statements and saves
  ;; huge indents.  Chose actions as a bitmask so that they don't overlap when we add them together.  This way we can check
  ;; for invalid multiple actions
  action = 0
  if keyword_set(calc) then $
     action += !pfo.calc
  if keyword_set(print) then $
     action += !pfo.print
  if keyword_set(indices) then $
     action += !pfo.indices
  if keyword_set(widget) then $
     action += !pfo.widget

  case action of
     !pfo.print : 
     !pfo.widget : 
     !pfo.indices :
     !pfo.calc : 
     else : message, 'ERROR: specify one of /CALC, /PRINT, /WIDGET, /INDICES'
  endcase ;; setting up for actions

  ;; Done checking the action switches.  In case we have some unforeseen problem, stop in this routine
  ON_ERROR, !tok.stop


  ;; Now catch errors that happen during our initial setup or that happen in the pfo_parinfo_parse loop code before the each
  ;; function is called.  The later should not happen.

  ;; CATCH: because we have to use CATCH to do basic program control (IDL resolve_routine and routine_info don't give
  ;; us graceful exit options), we can't have a global CATCH statement
  ;; that takes care of all our errors nicely.  We therefore have to reinitiate a CATCH condition at the top of each loop.
  ;; Start with one here.
  ;; Handle pfo_debug level.  CATCH errors if _not_ debugging
  if !pfo.debug le 0 then begin
     CATCH, err
     if err ne 0 then begin
        CATCH, /CANCEL
        return, pfo_parinfo_parse_catch(action, toprint, indices_idx, yaxis, error)
     endif ;; error
  endif ;; not debugging

  ;; Do some initial setup

  ;; For printing and widget display, we might want to have an introduction to the overall function (e.g. column headings).
  ;; We will pass this to the __print and __widget methods with call_function, below and then set it to 0.
  first_funct = 1

  case action of

     !pfo.print : begin
        ;; PRINTING
        action_method = '__print'
        ;; Check to see if we have anything to print
        if N_elements(parinfo) eq 0 then $
           return, 'NO FUNCTION'
        toprint = ''
     end ;; print

     !pfo.widget : begin
        ;; WIDGET
        action_method = '__widget'
        ;; pfo_parinfo_parse(/widget) is going to be called in a variety of circumstances: no existing top-level widget of
        ;; any kind, a parent widget, but no specific container and a container all set up to use with the pfo_cw_obj
        ;; clear_container system (see pfo_parinfo_cw_obj__define.pro)
        if N_elements(containerID) eq 0 then begin
           ;; We need to set up our own container in which the parinfo widgets will be displayed

           ;; Check to see if we need to create our own top-level base.  Do so in a way that doesn't muck with an undefined
           ;; parentID in the calling routine
           if N_elements(parentID_in) ne 0 then $
              parentID = parentID_in
           if N_elements(parentID) eq 0 then begin
              ;; We are basically just being a wrapper for pfo_parinfo_edit
              pfo_parinfo_edit, parinfo=parinfo, params=params_in, idx=idx, ispec=ispec, iROI=iROI, pfo_obj=pfo_obj, $
                                status_mask=status_mask, group_leader=group_leader, cw_obj=cw_obj, _EXTRA=extra
              ;; The return value should be the widget ID of the tlb, if the cw_obj is still valid
              if obj_valid(cw_obj) then $
                 return, cw_obj->tlbID() $
              else $
                 return, !tok.nowhere
              
           endif ;; Handing off to pfo_parinfo_edit

           ;; If we made it here, we have a parent in which the parinfo should be displayed, but we are not sure if a
           ;; container has been properly set up.  Use pfo_parinfo_container_cw to set up the container

           ;; To avoid excessive redraws of widgets, turn off update in parent widget until we are done (this turns off
           ;; update for all the widgets in the tlb)
           widget_control, parentID, update=0

           ;; Create the container into which the parinfo will be displayed.  We need to pass on any args that are necessary
           ;; for drawing the widget.  NOTE: if any of these args indicate that this instance of the pfo_parinfo_container_cw
           ;; should not be registered in the repopulate list (e.g. idx), the calling widget had better be listed there, or
           ;; else the widgets created here won't be in sink with the parinfo they are referencing.
           pfo_parinfo_container_cwID = $
              pfo_parinfo_container_cw(parentID, params=params_in, idx=idx, ispec=ispec, iROI=iROI, pfo_obj=pfo_obj, $
                                       status_mask=status_mask, $
                                       containerID=containerID, cw_obj=pfo_parinfo_container_cw_obj, $
                                       _EXTRA=extra)
           ;; Call ourselves recursively with our containerID.  This call is similar to the call pfo_parinfo_container_cw_obj
           ;; issues when it receives a repopulate request.  Note that the return request in this case is the list of 
           junk = pfo_parinfo_parse(/widget, parinfo=parinfo, params=params_in, idx=idx, ispec=ispec, iROI=iROI, pfo_obj=pfo_obj, $
                                    status_mask=status_mask, containerID=containerID, _EXTRA=extra)

           ;; Now that we are done putting all of the widgets into the container we just created, do one redraw.
           widget_control, parentID, update=1

           ;; Always return the ID of the top-most widget we have created here, since that is what the users will expect
           return, pfo_parinfo_container_cwID
        endif ;; Don't have a container yet
        
        ;; If we made it here, we should have been handed a container
        if NOT widget_info(containerID, /valid_ID) then $
           message, 'ERROR: invalid containerID.  This should not happen.  Are you using containerID in your calling routine?'

        ;; If we made it here, we have a valid containerID into which to dump our pfo function widgets.  Handle the case
        ;; where we have no function
        if N_elements(parinfo) eq 0 then begin
           rowID = widget_base(containerID, /row, /base_align_center)
           ID = pfo_finit_cw(rowID, pfo_obj=pfo_obj)
           return, rowID
        endif

     end ;; widget

     !pfo.indices : begin
        ;; INDICES
        action_method = '__indices'
        ;; Return !tok.nowhere if we have no
        if N_elements(parinfo) eq 0 then $
           return, !tok.nowhere
     end ;; indices

     !pfo.calc : begin
        ;; CALCULATING
        action_method = '__calc'
        ;; Initialize our error output
        yaxis = !values.d_NAN
        
        ;; Internal xaxis for calculations like dispersion relation.  Start from Xin.  Naturally raises an error if no Xin
        xaxis = Xin

        ;; Get our default Yaxis value from pfo_obj, if specified, otherwise from the pfo system variable
        init_Yaxis = !pfo.init_Yaxis
        if obj_valid(pfo_obj) then $
           pfo_obj->get_property, init_Yaxis=init_Yaxis

        ;; Initialize our Y axis.  Make sure it has the same dimensions and type as Xin
        yaxis = Xin
        yaxis[*] = init_Yaxis

        ;; Check to see if we have any calculation to do (note xaxis return keyword is properly initialized)
        if N_elements(parinfo) eq 0 then begin
           ;; Check to see if user wants ROI_Xin_idx return.  No parinfo means no ROI, no ROI means all Xin is fair game.
           if N_elements(ROI_Xin_idx) ne 0 or arg_present(ROI_Xin_idx) then $
              pfo_idx, Xin, ROI_Xin_idx
           return, yaxis
        endif ;; no parinfo

     end ;; /CALC

  endcase ;; setting up for actions

  ;; Make sure parinfo and params are defined.  Also keep params local.
  pfo_parinfo2params, parinfo, params, params_in

  ;; Use pfo_fidx to get indices
  use_idx = pfo_fidx(parinfo, idx=idx, npar=n_use, status_mask=status_mask)

  ;; Narrow with pfo_ROI_idx
  use_idx = pfo_ROI_idx(parinfo, idx=use_idx, ispec=ispec, iROI=iROI, allspec=allspec, allROI=allROI, count=n_use)


  ;; For efficiency sake, calculate ROI_Xaxis here if we are going to be working with ROIs that depend on Xaxis
  if action eq !pfo.calc and pfo_struct_tag_present(parinfo, 'pfo_ROI') then begin
     ;; Check to see if we have any ROIs.  Do this "in the raw" so that we can get the 
     ;; rest of the parameters we need for calculating the Xaxis.
     ROI_idx = where(floor(parinfo[use_idx].pfo.ftype) eq $
                     pfo_fnum('pfo_ROI', pfo_obj=pfo_obj), nROI_pars, $
                     complement=no_ROI_idx, ncomplement=n_non_ROI)
     if nROI_pars gt 0 then begin
        ;; unwrap
        ROI_idx = use_idx[ROI_idx]
        ;; Check to see if any ROIs are referenced to Xaxis
        junk = where(parinfo[ROI_idx].pfo.inaxis eq !pfo.Xaxis, nXaxis)
        if nXaxis gt 0 then begin
           ;; Use pfo_Xaxis to calc the Xaxis.  Make sure we
           ;; remove ROIs from the calculation to avoid an infinite
           ;; loop (I actually though of this before the first loop
           ;; occured... :-)
           ROI_Xaxis = Xin
           if n_non_ROI gt 0 then begin
              ;; unwrap
              no_ROI_idx = use_idx[no_ROI_idx]
              ;; pfo_Xaxis takes care of filtering out the no_ROI_idx that don't contribute to the Xaxis
              ROI_Xaxis = pfo_Xaxis(parinfo, Xin=Xin, params=params, idx=no_ROI_idx, $
                                    _EXTRA=extra)
           endif ;; some non-ROI parameters in the parinfo
        endif ;; some ROIs in Xaxis coordinates
     endif ;; some ROIs
  endif ;; calc setup of ROI_Xaxis

  ;; Now untangle the various levels of ispec, iROI, fseq, axes, etc.
  ;; This is the basic interpreter, so it is common to printing and
  ;; calculating, widget, and whatever else needs it.

  ;; Get ready to handle the Regions of Interested (ROIs) and multiple
  ;; spectra.  This happens on two levels.  (1) we have some ROI
  ;; functions laying around that aren't associated with any
  ;; particular functions (2) we have the pfo_ROI structure
  ;; appended, which enables that association.  Handle both cases.

  ;; Cycle through the ispecs.

  ;; Use pfo_uniq to get a reverse index list which allows easy access to groups of identical ispec.  NOTE: potentially easy
  ;; to miss variable naming convention: reverse index list from pfo_uniq is stored in arrays ending in _r_idx.  Indices
  ;; pulled out from the _r_idx just end in _idx.  In the case of ispec and iROI, to save memory, do this in two cases: one
  ;; if are using ROIs and one if we are not.
  if pfo_struct_tag_present(parinfo, 'pfo_ROI') then begin
     ;; This is going to be the general pattern of how we work through the parinfo.pfo[_ROI] structure
     junk = parinfo[use_idx].pfo_ROI.ispec
     junk = pfo_uniq(junk, bsort(junk), reverse_indices=ispec_r_idx, N_uniq=N_ispec)
     if N_ispec gt 1 then $
        message, 'ERROR: multiple ispecs are not yet supported'
  endif else begin
     ;; If we don't have a pfo_ROI, use pfo_uniq to get all the right answers, but try to do so with the minimum of memory
     junk = pfo_uniq(make_array(n_use, value=0B), reverse_indices=ispec_r_idx, N_uniq=N_ispec)
  endelse
  for iu_ispec=0, N_ispec-1 do begin
     ;; Catch at each loop level because of CATCH below
     if !pfo.debug le 0 then begin
        CATCH, err
        if err ne 0 then begin
           CATCH, /CANCEL
           return, pfo_parinfo_parse_catch(action, toprint, indices_idx, yaxis, error, containerID)
        endif ;; error
     endif ;; not debugging

     ;; Pick out the ith set of indices that point to identical values
     ispec_idx = ispec_r_idx[ispec_r_idx[iu_ispec]:ispec_r_idx[iu_ispec+1]-1]
     ;; unwrap so that these are indices into parinfo
     ispec_idx = use_idx[temporary(ispec_idx)]

     ;; Now check ROIs within this ispec.  Do this carefully, since we might have no ROIs, or a ROI that doesn't
     ;; overlap a function.
     have_ROIs = 0
     if pfo_struct_tag_present(parinfo, 'pfo_ROI') then begin
        ;; See if we have any ROI functions in this ispec
        junk = pfo_fidx(parinfo, 'pfo_ROI', idx=iROI_idx, nfunct=nfunct, pfo_obj=pfo_obj)
        if nfunct gt 0 then $
           have_ROIs = 1
        junk = parinfo[ispec_idx].pfo_ROI.iROI
        junk = pfo_uniq(junk, bsort(junk), reverse_indices=iROI_r_idx, N_uniq=N_iROI)
     endif else begin
        ;; If we don't have a pfo_ROI, use pfo_uniq to get all the right answers, but try to do so with the minimum of memory
        junk = pfo_uniq(make_array(n_use, value=0B), reverse_indices=iROI_r_idx, N_uniq=N_iROI)
     endelse

     ;; Loop over each iROI
     for iu_iROI=0, N_iROI-1 do begin
        ;; Catch at each loop level because of CATCH below
        if !pfo.debug le 0 then begin
           CATCH, err
           if err ne 0 then begin
              CATCH, /CANCEL
              return, pfo_parinfo_parse_catch(action, toprint, indices_idx, yaxis, error, containerID)
           endif ;; error
        endif ;; not debugging

        ;; Pick out the ith set of indices that point to identical values
        iROI_idx = iROI_r_idx[iROI_r_idx[iu_iROI]:iROI_r_idx[iu_iROI+1]-1]
        ;; unwrap so that these are indices into parinfo
        iROI_idx = ispec_idx[temporary(iROI_idx)]

        ;; Seach for ROI functions in our set of indices.  Note nROI eq 0 if no ROI functions were found * in this set of
        ;; iROI_idx *.  This would be weird, but not impossible
        ROI_idx = pfo_fidx(parinfo, 'pfo_ROI', idx=iROI_idx, nfunct=nROI, pfo_obj=pfo_obj)

        ;; The iROI = !pfo.allROI is a special case which lets us have multiple ROI functions across the X-axis _with the
        ;; same iROI_.  The advantage is that we can can assign functions to iROI=!pfo.allROI and have them over all of
        ;; those ROIs at once.  Otherwise, we would need to define the function on each ROI and link the parameters
        ;; together.  Check here to make sure the user isn't trying to have more than one ROI that is not !pfo.allROI
        if nROI gt 1 then begin
           if parinfo[ROI_idx[0]].pfo_ROI.iROI ne !pfo.allROI then $
              message, 'ERROR: more than one ROI function associated with iROI = ' + strtrim(parinfo[ROI_idx[0]].pfo_ROI.iROI, 2)
        endif ;; multiple ROIs with the same iROI

        ;; When we are calculating, set up the indices into Xin/xaxis.
        if action eq !pfo.calc then begin
           if have_ROIs eq 0 then begin
              ;; No ROIs means just use the whole Xin axis
              pfo_idx, Xin, Xin_ROI_idx
           endif else begin
              ;; We have ROIs somewhere in the parinfo.  Handle the unusual case of where we don't have a ROI
              ;; overlapping the current set of parameters marked with an iROI
              if nROI le 0 then $
                 CONTINUE

              ;; If we made it here, we have a ROI covering our function(s)

              ;; Get our ROI parameters in order.  We know that each ROI has the same number of parameters, so we can just
              ;; sort and reform
              if nROI gt 0 then begin
                 ;; This should put all of the lefts together and then rights.  IDL sort does not work consistently between
                 ;; platforms with respect to identical array values.  bsort from the IDLASTRO library always returns identical
                 ;; elements in the same order they were originally listed.  So with bsort, the first listed left ends up first,
                 ;; etc.  Reform then quickly puts the array in the format we want: each instance of the function occupies a column
                 ;; and the rows correspond to parameter numbers.  The array values themselves are indices into ROI_idx.
                 ROI_fidx_array = bsort(parinfo[ROI_idx].pfo.ftype)
                 ROI_fidx_array = reform(ROI_fidx_array, nROI, pfo_fnpars('pfo_ROI', pfo_obj=pfo_obj), /overwrite)
              endif ;; have some ROIs

              ;; Use pfo_ROI to get the indices into Xin/Xaxis for this ROI.  Make sure that pfo_ROI__fdefine is compiled,
              ;; since pfo_ROI__calc is likely hiding inside of it
              resolve_routine, 'pfo_ROI__fdefine', /no_recompile

              ;; Loop through our ROIs and build up the Xin_ROI_idx.  For "normal" ROIs, this loop only operates once.  But
              ;; there can be an arbitrary number of ROIs that are iROI = !pfo.allROI

              ;; Initialize Xin_ROI_idx for pfo_array_append
              Xin_ROI_idx = 0
              junk = temporary(Xin_ROI_idx)

              for itROI=0, nROI-1 do begin
                 t_Xin_ROI_idx = $
                    pfo_ROI__calc(Xin, params, parinfo=parinfo, $
                                  idx=ROI_idx[ROI_fidx_array[itROI, *]], $
                                  count=count, Xaxis=ROI_Xaxis, pfo_obj=pfo_obj)
                 ;; Check to see if the ROI resulted in any valid idx.  If not, skip to the next ROI
                 if count eq 0 then $
                    CONTINUE
                 ;; If we made it here, we have generated some valid points on Xin
                 pfo_array_append, Xin_ROI_idx, t_Xin_ROI_idx
              endfor ;; building up Xin_ROI_idx

              ;; Check to see if we have any valid Xin points in our ROI(s).  If not, just skip this iROI
              if N_elements(Xin_ROI_idx) eq 0 then $
                 CONTINUE

              ;; Take care of our ROI_Xin_idx output keyword, being polite not to build it up if the user doesn't want it.
              ;; If they specified it on the command line, the first time through it is either an undefined variable or a
              ;; variable they want reinitialized
              if NOT keyword_set(after_first_ROI_Xin_idx) and $
                 (N_elements(ROI_Xin_idx) ne 0 or arg_present(ROI_Xin_idx)) then begin
                 ;; Initilize ROI_Xin_idx for first and subsequent calls to pfo_array_append
                 ROI_Xin_idx = ''
              endif ;; initializing ROI_Xin_idx to a null list our first time around
              if N_elements(ROI_Xin_idx) ne 0 then begin
                 ;; Build up output ROI_Xin_idx
                 pfo_array_append, ROI_Xin_idx, Xin_ROI_idx
                 after_first_ROI_Xin_idx = 1
              endif ;; Building up output ROI_Xin_idx

           endelse ;; no ROIs vs ROIs
        endif ;; calc ROI setup


        ;; Call the functions by sequence within each ROI
        junk = parinfo[iROI_idx].pfo.fseq
        junk = pfo_uniq(junk, bsort(junk), reverse_indices=fseq_r_idx, N_uniq=N_fseq)
        for iseq=0, N_fseq-1 do begin
           ;; Catch at each loop level because of CATCH below
           if !pfo.debug le 0 then begin
              CATCH, err
              if err ne 0 then begin
                 CATCH, /CANCEL
                 return, pfo_parinfo_parse_catch(action, toprint, indices_idx, yaxis, error, containerID)
              endif ;; error
           endif ;; not debugging

           ;; Pick out the ith set of indices that point to identical values
           fseq_idx = fseq_r_idx[fseq_r_idx[iseq]:fseq_r_idx[iseq+1]-1]
           ;; unwrap so that these are indices into parinfo
           fseq_idx = iROI_idx[temporary(fseq_idx)]

           ;; Decend to the next level: outaxis
           junk = parinfo[fseq_idx].pfo.outaxis
           junk = pfo_uniq(junk, bsort(junk), reverse_indices=outaxis_r_idx, N_uniq=N_outaxis)
           for ioutaxis=0, N_outaxis-1 do begin
              ;; Catch at each loop level because of CATCH below
              if !pfo.debug le 0 then begin
                 CATCH, err
                 if err ne 0 then begin
                    CATCH, /CANCEL
                    return, pfo_parinfo_parse_catch(action, toprint, indices_idx, yaxis, error, containerID)
                 endif ;; error
              endif ;; not debugging
              ;; Pick out the ith set of indices that point to identical values
              outaxis_idx = outaxis_r_idx[outaxis_r_idx[ioutaxis]:outaxis_r_idx[ioutaxis+1]-1]
              ;; unwrap so that these are indices into parinfo
              outaxis_idx = fseq_idx[temporary(outaxis_idx)]


              ;; Operation
              junk = parinfo[outaxis_idx].pfo.fop
              junk = pfo_uniq(junk, bsort(junk), reverse_indices=fop_r_idx, N_uniq=N_fop)
              for ifop=0, N_fop-1 do begin
                 ;; Set a flag for axis replacement, to make sure we don't have more than one in an fseq
                 n_repl = 0

                 ;; Catch at each loop level because of CATCH below
                 if !pfo.debug le 0 then begin
                    CATCH, err
                    if err ne 0 then begin
                       CATCH, /CANCEL
                       return, pfo_parinfo_parse_catch(action, toprint, indices_idx, yaxis, error, containerID)
                    endif ;; error
                 endif ;; not debugging
                 fop_idx = fop_r_idx[fop_r_idx[ifop]:fop_r_idx[ifop+1]-1]
                 fop_idx = outaxis_idx[temporary(fop_idx)]

                 ;; inaxis
                 junk = parinfo[fop_idx].pfo.inaxis
                 junk = pfo_uniq(junk, bsort(junk), reverse_indices=inaxis_r_idx, N_uniq=N_inaxis)
                 for iinaxis=0, N_inaxis-1 do begin
                    ;; Catch at each loop level because of CATCH below
                    if !pfo.debug le 0 then begin
                       CATCH, err
                       if err ne 0 then begin
                          CATCH, /CANCEL
                          return, pfo_parinfo_parse_catch(action, toprint, indices_idx, yaxis, error, containerID)
                       endif ;; error
                    endif ;; not debugging
                    inaxis_idx = inaxis_r_idx[inaxis_r_idx[iinaxis]:inaxis_r_idx[iinaxis+1]-1]
                    inaxis_idx = fop_idx[temporary(inaxis_idx)]

                    ;; Functions: Recall that ftype is a real number, with the integer determining the function type and
                    ;; the decimal the parameter of that function.  We want to work with the integer part, the fnum.
                    ;; Save them for use below
                    fnums = floor(parinfo[inaxis_idx].pfo.ftype)
                    junk = pfo_uniq(fnums, bsort(fnums), reverse_indices=fnum_r_idx, N_uniq=N_fnums)
                    for ifnum=0, N_fnums-1 do begin
                       ;; Catch at each loop level because of CATCH below
                       if !pfo.debug le 0 then begin
                          CATCH, err
                          if err ne 0 then begin
                             CATCH, /CANCEL
                             return, pfo_parinfo_parse_catch(action, toprint, indices_idx, yaxis, error, containerID)
                          endif ;; error
                       endif ;; not debugging
                       fnum_idx = fnum_r_idx[fnum_r_idx[ifnum]:fnum_r_idx[ifnum+1]-1]
                       fnum_idx = inaxis_idx[temporary(fnum_idx)]

                       ;; Collect some information on our function.  Remember to reset fname, or else finfo will
                       ;; complain....
                       fnum = fnums[ifnum]
                       fname = ''
                       pfo_finfo, fnum=fnum, fname=fname, fnpars=fnpars, pfo_obj=pfo_obj

                       ;; As advertised, the last thing used for unique identification is pfoID.  If the user is
                       ;; careful to assign a unique pfoID to each set of parameters that represent a function, the
                       ;; parameter list can be totally randomized and we can put it back together again here
                       junk = parinfo[fnum_idx].pfo.pfoID
                       junk = pfo_uniq(junk, bsort(junk), reverse_indices=ID_r_idx, N_uniq=N_IDs)
                       for iID=0, N_IDs-1 do begin
                          ;; Catch at each loop level because of CATCH below
                          if !pfo.debug le 0 then begin
                             CATCH, err
                             if err ne 0 then begin
                                CATCH, /CANCEL
                                return, pfo_parinfo_parse_catch(action, toprint, indices_idx, yaxis, error, containerID)
                             endif ;; error
                          endif ;; not debugging
                          ID_idx = ID_r_idx[ID_r_idx[iID]:ID_r_idx[iID+1]-1]
                          ID_idx = fnum_idx[temporary(ID_idx)]
                          ;; Even with all this work, there still might be multiple instances of function fnum.  npar is
                          ;; the total number of parameters of function fnum
                          npar = N_elements(ID_idx)

                          ;; If we know the number of parameters of our function, we are able to take a decent guess at
                          ;; putting multiple instances of a function together.  NOTE: parameters will be assembled in the
                          ;; order in which they are listed in the original parinfo array
                          if fnpars ne 0 then begin
                             ;; Check to see that we have the right number of parameters
                             if npar mod fnpars ne 0 then $
                                message, 'ERROR: incorrect number of parameters for function type ' + fname + ': ' + strtrim(npar, 2)
                             

                             ;; This should put all of the decimal ftypes of the same kind together (see similar code in
                             ;; ROI section, above).  The way bsort works, the each individual instance of an ftype gets
                             ;; listed listed in the order they were originally found in the parinfo.
                             fidx_array = bsort(parinfo[ID_idx].pfo.ftype)
                             ;; Having stacked up all of the indices in a long line in memory, in decimal ftype order, use
                             ;; reform to just pretend that each row corresponds to a parameter (IDL is a row major
                             ;; language) and each column to an instance of the function
                             fidx_array = reform(fidx_array, npar/fnpars, fnpars, /overwrite)
                          endif else begin
                             ;; This is the fnpars = 0 case, which signals that we don't know the number of parameters our
                             ;; function is supposed to have.  In this case, we can only assume that all of the parameters
                             ;; of this fnum and ID belong to one instance of the function.  Put together fidx_array in
                             ;; the same row-major way we do for multiple instances of the same function
                             fidx_array = transpose(indgen(npar, type=size(/type, npar)))
                          endelse
                          ;; The number of functions is the number of columns we have in our fidx_array
                          nfunct = (size(/dimensions, fidx_array))[0]

                          ;; Call the __<action> functions in parray one at a time.
                          for iIDfn=long(0), nfunct-1 do begin
                             ;; Catch at each loop level because of CATCH below
                             if !pfo.debug le 0 then begin
                                CATCH, err
                                if err ne 0 then begin
                                   CATCH, /CANCEL
                                   return, pfo_parinfo_parse_catch(action, toprint, indices_idx, yaxis, error, containerID)
                                endif ;; error
                             endif ;; not debugging

                             ;; Assemble parameters for this particular instance of the function.  Reform is necessary to
                             ;; remove vestigial dimensions
                             fidx = reform(ID_idx[fidx_array[iIDfn, *]])

                             ;; Check to see if we have more than one replacement function 
                             if parinfo[fidx[0]].pfo.fop eq !pfo.repl then $
                                n_repl += 1
                             if n_repl gt 1 then $
                                message, 'ERROR: more than one function with the same fseq is replacing the outaxis'

                             ;; Make a catch in case the local copy of pfo doesn't have this __fdefine
                             CATCH, err
                             if err ne 0 then begin
                                CATCH, /CANCEL
                                message, /CONTINUE, 'ERROR: file ' + fname + '__fdefine.pro not found.  Skipping this function with possibly disasterous results....'
                                CONTINUE
                             endif ;; catching missing __fdefine

                             ;; Create a command that calls the pfo primitive.  First, make sure that all methods for this
                             ;; primitive are compiled.  This assumes that they are all collected into fname__fdefine.
                             ;; The user may not have created the parinfo with pfo_parinfo_new in this session
                             ;; --> I might need a catch here
                             resolve_routine, fname + '__fdefine', /no_recompile

                             ;; Now call fname__action.  If that doesn't exist, call pfo_null__action.  This allows
                             ;; pfo_null to be a fallback for things like printing and widget for most simple functions.
                             ;; CATCH makes this work, but the code reads backwards: else executes first.
                             CATCH, err
                             if err ne 0 then begin
                                CATCH, /CANCEL
                                funct_name = 'pfo_null' + action_method
                             endif else begin
                                ;; Use IDL's routine_info to raise an error if fname__action_method is not found.
                                ;; Unfortunately, this bombs if the routine has yet to be compiled, hence the need to
                                ;; resolve_routine.  We do resolve_routine on two levels.  One assuming that all of the
                                ;; fname code is in fname__fdefine.pro (above) and once assuming the individual methods
                                ;; are separate.
                                funct_name = fname + action_method
                                resolve_routine, funct_name, /no_recompile, /either
                                init_params = routine_info(funct_name, /functions, /parameters)
                             endelse ;; first time through checking for fname__action
                             ;; Turn off catching or else when we are in debug mode, this catch will activate (no matter
                             ;; where we are in the routine).
                             CATCH, /CANCEL

                             ;; Catch errors in our primitive functions unless the user is in PFO debugging mode
                             if !pfo.debug le 0 then begin
                                CATCH, err
                                if err ne 0 then begin
                                   CATCH, /CANCEL
                                   ;; For actions that result in text output, put error in text
                                   case action of 
                                      !pfo.print : begin
                                         toprint = toprint + ' %PFO_PARINFO_PARSE: ERROR: Caught the following error while calling ' + funct_name + ': ' + !error_state.msg + ')'
                                      end
                                      !pfo.widget : begin
                                         junk = widget_text(containerID, value='Caught the following error while calling ' + funct_name + ': ' + !error_state.msg)
                                      end
                                      else:
                                   endcase ;; putting error into text output methods
                                   ;; For all actions, print message to terminal
                                   message, /CONTINUE, /NONAME, !error_state.msg
                                   message, /CONTINUE, 'ERROR: caught the above error while calling ' + funct_name + '.  Ignoring this function and trying to continue'
                                   CONTINUE
                                endif ;; caught error
                             endif  ;; not debugging

                             ;; CALL FUNCTION
                             case action of 

                                !pfo.calc : begin
                                   ;; Make a temporary variable, x, which is the ROI-specific input axis of the function
                                   ;; we are going to call
                                   case parinfo[fidx[0]].pfo.inaxis of
                                      !pfo.none    : x = !values.d_nan
                                      !pfo.Xin     : x = Xin[Xin_ROI_idx]
                                      !pfo.Xaxis   : x = xaxis[Xin_ROI_idx]
                                      !pfo.Yaxis   : x = yaxis[Xin_ROI_idx]
                                      else : message, 'ERROR: unrecognized inaxis value ' + string(inaxis)
                                   endcase

                                   ;; Do our X-axis transformation
                                   if keyword_set(parinfo[fidx[0]].pfo.infunct) then $
                                      x = call_function(parinfo[fidx[0]].pfo.infunct, x)

                                   ;; Call our PFO function
                                   fy = $
                                      call_function( $
                                      funct_name, $
                                      temporary(x), params, $
                                      parinfo=parinfo, idx=fidx, $
                                      status_mask=status_mask, $
                                      pfo_obj=pfo_obj, $
                                      _EXTRA=extra)

                                   ;; Do our simple tranformation on the
                                   ;; output axis, if specified
                                   if keyword_set(parinfo[fidx[0]].pfo.outfunct) then $
                                      fy = call_function(parinfo[fidx[0]].pfo.outfunct, fy)
                                   
                                   ;; Make a temporary variable, y, which is the ROI-specific output on "outaxis" of all
                                   ;; functions which have operated so far.
                                   case parinfo[fidx[0]].pfo.outaxis of
                                      !pfo.none    : y = !values.d_nan
                                      !pfo.Xin     : message, 'ERROR: cannot write to input axis'
                                      !pfo.Xaxis   : y = xaxis[Xin_ROI_idx]
                                      !pfo.Yaxis   : y = yaxis[Xin_ROI_idx]
                                      else : message, 'ERROR: unrecognized outaxis value ' + string(oaxis)
                                   endcase

                                   ;; OPERATION: Combine the output of our function with y, the output of all previous
                                   ;; functions operating on "outaxis"
                                   case parinfo[fidx[0]].pfo.fop of 
                                      !pfo.none	: ; no output
                                      !pfo.repl	: y = temporary(fy)
                                      !pfo.add	: y = temporary(y) + temporary(fy)
                                      !pfo.mult	: y = temporary(y) * temporary(fy)
                                      !pfo.convol: y = $
                                         convol(temporary(y), temporary(fy), center=convol_center, _EXTRA=extra)
                                      else	: message, 'ERROR: unrecognized operation ' + string(fop)
                                   endcase ;; fop

                                   ;; Put y into the desired output axis
                                   case parinfo[fidx[0]].pfo.outaxis of
                                      !pfo.none	: ; no change in y
                                      !pfo.Xin	: message, 'ERROR: cannot write to input axis'
                                      !pfo.Xaxis	: xaxis[Xin_ROI_idx] = temporary(y)
                                      !pfo.Yaxis	: yaxis[Xin_ROI_idx] = temporary(y)
                                      else	: message, 'ERROR: unrecognized inaxis value ' + string(inaxis)
                                   endcase ;; oaxis
                                end ;; calc

                                !pfo.print : begin
                                   ;; Build our our "toprint" output string
                                   toprint += $
                                      call_function( $
                                      funct_name, parinfo, params=params, $
                                      idx=fidx, $
                                      status_mask=status_mask, $
                                      fname=fname, first_funct=first_funct, $
                                      pfo_obj=pfo_obj, $
                                      _EXTRA=extra)
                                end ;; print

                                !pfo.widget : begin
                                   ;; Create our widgets and build up our list of IDs and cw_objs
                                   ID = $
                                      call_function( $
                                      funct_name, containerID, $
                                      params=params, $
                                      idx=fidx, $
                                      status_mask=status_mask, $
                                      fname=fname, first_funct=first_funct, $
                                      pfo_obj=pfo_obj, $
                                      cw_obj=cw_obj, $
                                      _EXTRA=extra)
                                   pfo_array_append, widget_IDs, ID
                                   pfo_array_append, cw_objs, cw_obj
                                end ;; widget

                                !pfo.indices: begin
                                   ;; Get the sorted (or otherwise ordered) idx for this function
                                   sidx = $
                                      call_function( $
                                      funct_name, $
                                      parinfo, idx=fidx, $
                                      status_mask=status_mask, $
                                      fname=fname, first_funct=first_funct, $
                                      pfo_obj=pfo_obj, $
                                      terminate_idx=terminate_idx, $
                                      _EXTRA=extra)

                                   ;; Check to see if the user wants to expand an idx into all of the idx of that
                                   ;; particular function.  If the user does not want to do that, just accumulate all of
                                   ;; the idx (see else)
                                   if N_elements(expand_idx) gt 0 then begin
                                      ;; Use a flag variable to determine if one or more indices in expand_idx are found
                                      ;; in this function.  Do it this way so we don't get multiple copies of fidx!
                                      expand_this_funct = 0
                                      for ie=0, N_elements(expand_idx)-1 do begin
                                         junk = where(expand_idx[ie] eq fidx, count)
                                         if count gt 0 then $
                                            expand_this_funct = 1
                                      endfor ;; checking all expand_idx against this function
                                      if keyword_set(expand_this_funct) then $
                                         pfo_array_append, indices_idx, sidx
                                   endif else begin
                                      ;; If we are not expanding a particular (set of) idx, just accumulate our set of
                                      ;; parsed fidx in indices_idx.  NOTE: individual functions are responsible for
                                      ;; adding terminate_idx to the end of their
                                      pfo_array_append, indices_idx, sidx
                                   endelse 
                                end ;; indices

                                else :
                             endcase ;; action
                             ;; Set our first_funct flag to 0, since we are done with it
                             first_funct = 0
                          endfor  ;; each instance of fnum with this ID (iIDfn)
                       endfor ;; iID
                    endfor ;; ifnum
                 endfor ;; ifop
              endfor ;; ioutaxis
           endfor ;; iinaxis
        endfor ;; iseq
     endfor ;; iu_iROI (each iROI)
  endfor ;; iu_ispec


  ;; Return value depends on our action
  case action of
     !pfo.print : return, toprint
     !pfo.widget : return, widget_IDs
     !pfo.indices : return, indices_idx
     !pfo.calc : begin
        ;; Handle case where caller wanted ROI_Xin_idx but none were found
        if arg_present(ROI_Xin_idx) and N_elements(ROI_Xin_idx) eq 0 then $
           ROI_Xin_idx = !tok.nowhere
        return, yaxis
     end
     else :
  endcase

end
